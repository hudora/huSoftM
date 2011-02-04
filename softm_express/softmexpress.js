(function() {
  var args, colors, crypto, desthost, destport, http, httpProxy, listenport, login_required, password, query_counter, querystring, select, sendReply, server, startswith, url, util, welcome, x_en;
  colors = require('./lib/colors');
  crypto = require('crypto');
  http = require('http');
  httpProxy = require('./lib/node-http-proxy');
  querystring = require('querystring');
  url = require('url');
  util = require('util');
  welcome = '      __             _         _   _                                          \n    /    )         /  `        /  /|                                          \n----\\--------__--_/__---_/_---/| /-|----__---|/------__---)__----__---__---__-\n     \\     /   ) /      /    / |/  |  /___)  |     /   ) /   ) /___) (_ ` (_ `\n_(____/___(___/_/______(_ __/__/___|_(___ __/|____/___/_/_____(___ _(__)_(__)_\n           Midrange over HTTP                    /     /                      \n                                                /                             ';
  util.puts(welcome.yellow.bold);
  args = process.argv.slice(2);
  password = args[0] || 'geheim';
  desthost = args[1] || 'localhost';
  destport = args[2] || '8000';
  listenport = args[3] || '8082';
  query_counter = 0;
  sendReply = function(response, code, message) {
    response.writeHead(code, {
      "Content-Type": 'text/plain',
      "Server": "SoftMexpress/Node.js/" + process.version + " " + process.platform,
      "Date": (new Date()).toUTCString()
    });
    response.write(message);
    response.write("\n");
    return response.end();
  };
  login_required = function(request, response, handler) {
    var digest, encoding, hmac;
    console.log(request.url);
    console.log(url.parse(request.url));
    hmac = crypto.createHmac('sha1', password);
    hmac.update(request.url);
    digest = hmac.digest(encoding = 'hex');
    if (request.headers['x-sig'] !== digest) {
      console.log(request.headers['x-sig']);
      return sendReply(response, 401, "Not with me!");
    } else {
      return handler(request, response);
    }
  };
  select = function(request, response) {
    var newurl, parsedurl, proxy, query, querystr;
    parsedurl = url.parse(request.url);
    query = JSON.parse(querystring.parse(parsedurl.query).q);
    querystr = "SELECT " + query.fields.join(',') + " FROM " + query.tablenames.join(',');
    if (query.joins) {
      query.joins.forEach(function(x) {
        var jointable, leftattr, rightattr;
        jointable = x[0];
        leftattr = x[1];
        rightattr = x[2];
        return querystr = querystr + ' LEFT OUTER JOIN ' + jointable + ' ON ' + leftattr + '=' + rightattr;
      });
    }
    if (query.condition) {
      querystr = querystr + ' WHERE ' + query.condition.replace(/';/g, "");
    }
    if (query.grouping) {
      querystr = querystr + ' GROUP BY ' + query.grouping.join(',').replace(/';/g, "");
    }
    if (query.ordering) {
      querystr = querystr + ' ORDER BY ' + query.ordering.join(',').replace(/';/g, "");
    }
    if (query.limit) {
      querystr = querystr + ' FETCH FIRST ' + (query.limit + '').replace(/';/g, "") + ' ROWS ONLY';
    }
    console.log(request.client.remoteAddress + ': ' + querystr);
    newurl = '/select?' + querystring.stringify({
      query: querystr,
      tag: query.tag + '+sEx'
    });
    request.url = newurl;
    proxy = new httpProxy.HttpProxy(request, response);
    proxy.proxyRequest(destport, desthost);
    return query_counter += 1;
  };
  x_en = function(request, response) {
    var newurl, parsedurl, proxy, query, querystr, table, tablemapping, value;
    tablemapping = {
      ISA00: ['IASTAT', 'X'],
      ISB00: ['IBSTAT', 'X'],
      ISK00: ['IKSTAT', 'X'],
      ISR00: ['IRSTAT', 'X'],
      ISZ00: ['IZSTAT', 'X'],
      ALK00: ['LKKZ02', 1]
    };
    parsedurl = url.parse(request.url);
    query = JSON.parse(querystring.parse(parsedurl.query).q);
    if (TABLEMAPPING[query.tablename] === void 0) {
      sendReply(response, 404, "Unknown tablename!");
    } else {
      table = TABLEMAPPING[query.tablename][0];
      value = TABLEMAPPING[query.tablename][1];
      querystr = "UPDATE " + +" SET " + table + " = '" + value + "' WHERE " + query.condition.replace(/';/g, "");
    }
    console.log(req.client.remoteAddress + ': ' + querystr);
    newurl = '/update?' + querystring.stringify({
      query: querystr,
      tag: query.tag + '+sEx'
    });
    req.url = newurl;
    proxy = new httpProxy.HttpProxy(request, response);
    return proxy.proxyRequest(destport, desthost);
  };
  startswith = function(s1, s2) {
    return s1.substr(0, s2.length) === s2;
  };
  server = httpProxy.createServer(function(request, response) {
    var parsedurl, proxy;
    parsedurl = url.parse(request.url);
    if (parsedurl.pathname === '/info' && request.method !== 'GET') {
      request.url = '/info';
      proxy = new httpProxy.HttpProxy(request, response);
      return proxy.proxyRequest(destport, desthost);
    } else if (parsedurl.pathname === '/stats' && request.method === 'GET') {
      return sendReply(response, 200, "query_counter: " + query_counter);
    } else if (startswith(parsedurl.pathname, '/sql')) {
      if (request.method !== 'GET') {
        return sendReply(response, 405, "Method not allowed");
      } else {
        return login_required(request, response, select);
      }
    } else if (startswith(parsedurl.pathname, '/x_en')) {
      if (request.method !== 'GET') {
        return sendReply(response, 405, "Method not allowed");
      } else {
        return login_required(request, response, x_en);
      }
    } else {
      return sendReply(response, 404, "Not here!");
    }
  });
  server.listen(listenport);
  util.puts('proxy server '.blue + 'started '.green.bold + 'on port '.blue + (listenport + '').yellow);
  util.puts(' connecting to '.blue + (desthost + ':' + destport).yellow);
}).call(this);
