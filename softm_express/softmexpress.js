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
 * Überprüfe Credentials
 * @param {Object} req
 * @param {Object} res
 * @param {Object} handler
 */
function login_required(req, res, handler) {
	hmac = crypto.createHmac('sha1', password);
	hmac.update(req.url);
	digest = hmac.digest(encoding='hex');
	
	if(req.headers['x-sig'] == digest) {
		console.log(request.headers['x-sig']);
        console.log(digest);
        response.writeHead(401, {
                        "Content-Type" : 'text/plain',
                        "Server" : "SoftMexpress/Node.js/" + process.version +  " " + process.platform,
                        "Date" : (new Date()).toUTCString()
                    });
        response.write("Not with me!\n");
        response.end();
	} else {
		handler(req, res);
	}
}


/**
 * Führe SQL-SELECT aus
 * @param {Object} request
 * @param {Object} response
 */
function select(request, response) {

    query = JSON.parse(querystring.parse(parsedurl.query).q);
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
    })
	
    req.url = newurl;
    var proxy = new httpProxy.HttpProxy(req, res);
    proxy.proxyRequest(destport, desthost);
}


/**
 * Setze Satzstatus auf 'X'
 * @param {Object} req
 * @param {Object} res
 */
function ex(req, res) {
	query = JSON.parse(querystring.parse(parsedurl.query).q);
	querystr = "UPDATE " + query.tablename + " SET " + query.field + " = 'X'";
	if(query.condition) {
		querystr = querystr + ' WHERE ' + query.condition.replace(/';/g, "");
	}
	console.log(req.client.remoteAddress + ': ' + querystr);
	newurl = '/update?' + querystring.stringify({
		query: querystr,
		tag: query.tag + '+sEx'
	});
	req.url = newurl;
	var proxy = new httpProxy.HttpProxy(req, res);
    proxy.proxyRequest(destport, desthost);
}

/**
 * Very Simple URL mapper
 */
MAPPING = {}
function getHandler(path, method) {	
	var tmp = MAPPING[method];
	if(tmp === undefined) {
		return undefined;
	}
	return tmp[path];
}
function addHandler(path, method, handler) {
	if(MAPPING[method] === undefined) {
		MAPPING[method] = {}
	}
	MAPPING[method][path] = handler;
}

addHandler('/info', 'GET', function(req, res) {
	login_required(req, res, info)
});
addHandler('/sql', 'GET', function(req, res) {
	login_required(req, res, select);
});
addHandler('/ex', 'POST', function(req, res) {
	login_required(req, res, ex);
});


httpProxy.createServer(function (req, res) {
  var parsedurl = url.parse(req.url);
  var path = parsedurl.pathname;

  handler = getHandler(path, req.method);
  if(handler === undefined) {
          res.writeHead(404, {
                    "Content-Type" : 'text/plain',
                    "Server" : "SoftMexpress/Node.js/" + process.version +  " " + process.platform,
                    "Date" : (new Date()).toUTCString()
                });
          res.write("Not here!\n");
          res.end();
   } else {
   	handler(req, res);
   }
}).listen(listenport);

util.puts('proxy server '.blue + 'started '.green.bold + 'on port '.blue + (listenport + '').yellow + ' connecting to '.blue + (desthost + ':' + destport).yellow);
