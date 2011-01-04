/*
"""husoftm is a toolkit for accessing a AS/400 running SoftM Suite.

node.js proxy for connectiong huSoftM Python clients to the erlang based odbc_bridge.

This proxy is meant to run on a gateway and receive query descriptions, verify their
authenticity by checking a HMAC, generating an SQL query and passing this to odbc_bridge
which in turn queries an AS/400. Answers are passed back on the same way.

Keep in mind that the HMAC does not protect against replay attacks - although this shouldn't
be a big issue since all of this functions can be assumed to be idempotent.

to use you need node.js installed

    node softmexpress.js s3kri1t odbc_bridge.local.example.com 8000 8082

branched of connection2.py, Maximillian Dornseif on 2010-11-28.
Copyright (c) 2010 HUDORA. All rights reserved.
all the heavy lifting is done by https://github.com/nodejitsu/node-http-proxy
*/

var util = require('util'),
    http = require('http'),
    url = require('url'),
    colors = require('./lib/colors'),
    crypto = require('crypto'),
    querystring = require('querystring'),
    pool = require('./lib/pool'),
    httpProxy = require('./lib/node-http-proxy');


var welcome = '\
      __             _         _   _                                          \n\
    /    )         /  `        /  /|                                          \n\
----\\--------__--_/__---_/_---/| /-|----__---|/------__---)__----__---__---__-\n\
     \\     /   ) /      /    / |/  |  /___)  |     /   ) /   ) /___) (_ ` (_ `\n\
_(____/___(___/_/______(_ __/__/___|_(___ __/|____/___/_/_____(___ _(__)_(__)_\n\
           Midrange over HTTP                    /     /                      \n\
                                                /                             \n';
util.puts(welcome.yellow.bold);

var args = process.argv.slice(2);
var password = args[0] || 'geheim';
var desthost = args[1] || 'localhost';
var destport = args[2] || '8000';
var listenport = args[3] || '8082';

/**
 * Very Simple URL mapper
 */
var URLMAP = {};
function getHandler(path, method) {
	var tmp = URLMAP[method];
	if(tmp === undefined) {
		return undefined;
	}
	return tmp[path];
}

function addHandler(path, method, handler) {
	if(URLMAP[method] === undefined) {
		URLMAP[method] = {};
	}
	URLMAP[method][path] = handler;
}

/**
 * Write error message and return
 */
function error(req, res, code, msg) {
    res.writeHead(code, {
              "Content-Type" : 'text/plain',
              "Server" : "SoftMexpress/Node.js/" + process.version +  " " + process.platform,
              "Date" : (new Date()).toUTCString()
          });
    res.write(msg);
    res.end();
}

/**
 * Überprüfe Credentials
 */
function login_required(req, res, handler) {
	hmac = crypto.createHmac('sha1', password);
	hmac.update(req.url);
	digest = hmac.digest(encoding='hex');
	
	if(req.headers['x-sig'] == digest) {
		console.log(request.headers['x-sig']);
        console.log(digest);
        error(req, res, 401, "Not with me!\n");
	} else {
		handler(req, res);
	}
}

/**
 * Führe SQL-SELECT aus
 */
function select(request, response) {
    var parsedurl = url.parse(req.url);
    var query = JSON.parse(querystring.parse(parsedurl.query).q);
    // build the SQL query
    querystr = "SELECT " + query.fields.join(',') + " FROM " + query.tablenames.join(',');
    if (query.joins) {
        query.joins.forEach(function(x){
            jointable = x[0];
            leftattr = x[1];
            rightattr = x[2];
            querystr = querystr + ' LEFT OUTER JOIN ' + jointable + ' ON ' + leftattr + '=' + rightattr;
        });
    }
    if (query.condition) 
        querystr = querystr + ' WHERE ' + query.condition.replace(/';/g, "");
    if (query.grouping) 
        querystr = querystr + ' GROUP BY ' + query.grouping.join(',').replace(/';/g, "");
    if (query.ordering) 
        querystr = querystr + ' ORDER BY ' + query.ordering.join(',').replace(/';/g, "");
    if (query.limit) 
        querystr = querystr + ' FETCH FIRST ' + (query.limit + '').replace(/';/g, "") + ' ROWS ONLY';
    console.log(req.client.remoteAddress + ': ' + querystr);
    newurl = '/select?' + querystring.stringify({
        query: querystr,
        tag: query.tag + '+sEx'
    });
	
    req.url = newurl;
    var proxy = new httpProxy.HttpProxy(req, res);
    proxy.proxyRequest(destport, desthost);
}

/**
 * Setze Satzstatus auf 'X'
 */
function x(req, res) {
    
    /* Mapping from tablename to status field name
     * These are the only tables that can be used to 'x' a record
     */
    var TABLEMAPPING = {
        'ISA00': 'IASTAT',
        'ISB00': 'IBSTAT',
        'ISK00': 'IKSTAT',
        'ISR00': 'IRSTAT',
        'ISZ00': 'IZSTAT'
    };
    
    var parsedurl = url.parse(req.url);
	query = JSON.parse(querystring.parse(parsedurl.query).q);
	
	var tablename = query.tablename;
	if(TABLEMAPPING[tablename] === undefined) {
	    error(req, res, 404, "Unknown tablename!\n");
	    return;
	}
	
	var querystr = "UPDATE " +  + " SET " + TABLEMAPPING[tablename] + " = 'X' WHERE " + query.condition.replace(/';/g, "");
	console.log(req.client.remoteAddress + ': ' + querystr);
	newurl = '/update?' + querystring.stringify({
		query: querystr,
		tag: query.tag + '+sEx'
	});
	req.url = newurl;
	var proxy = new httpProxy.HttpProxy(req, res);
    proxy.proxyRequest(destport, desthost);
}

/* Add handlers and start server */
addHandler('/info', 'GET', function(req, res) {
	login_required(req, res, info);
});
addHandler('/sql', 'GET', function(req, res) {
	login_required(req, res, select);
});
addHandler('/x', 'POST', function(req, res) {
	login_required(req, res, x);
});

httpProxy.createServer(function (req, res) {
  var parsedurl = url.parse(req.url);
  var path = parsedurl.pathname;

  console.log(req.client.remoteAddress + ': ' + req.method + " " + path);

  handler = getHandler(path, req.method);

  if(handler === undefined) {
      error(req, res, 404, 'Not here!\n');
  } else {
      handler(req, res);
  }
}).listen(listenport);

util.puts('proxy server '.blue + 'started '.green.bold + 'on port '.blue + (listenport + '').yellow + ' connecting to '.blue + (desthost + ':' + destport).yellow);
