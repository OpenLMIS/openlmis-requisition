const fs = require('fs');
const ip = require('ip');
const http = require('http');
const uuid = require('uuid');
const deasync = require('deasync');
const raml = require('raml-parser');
const commandLineArgs = require('command-line-args');

function ServiceRAMLParser() {
  var self = this;

  self.extractResources = function(filename) {
    // Parse RAML file to retrieve available resources list
    return raml.loadFile(filename).then(function(data) {
      var resources = [];
      data.resources.forEach(function(node) {
        self.extractNodeResources(node).forEach(function(resource) {
          resources.push(resource);
        })
      });
      return resources;
    });
  }

  self.extractNodeResources = function(node) {
    var paths = [];

    if (node.resources) {
      node.resources.forEach(function(childNode) {
        self.extractNodeResources(childNode).forEach(function(path) {
          paths.push(node.relativeUri + path);
        });
      });
    }

    if (node.methods instanceof Array && node.methods.length > 0){
      paths.push(node.relativeUri);
    }

    return paths;
  }
}
function ServiceConsulRegistrator(host, port) {
  self = this;
  self.host = host;
  self.port = port;

  self.registerService = function(serviceData) {
    return registerServiceBase(serviceData, 'PUT');
  }

  self.deregisterService = function(serviceData) {
    return registerServiceBase(serviceData, 'DELETE');
  }

  self.registerResources = function(serviceData, resourceArray) {
    return registerResourcesBase(serviceData, resourceArray, 'PUT');
  }

  self.deregisterResources = function(serviceData, resourceArray) {
    return registerResourcesBase(serviceData, resourceArray, 'DELETE');
  }

  function registerServiceBase(serviceData, mode) {
    var requestPath = mode === 'PUT' ? 'register' : 'deregister/' + serviceData.ID;
    var request = http.request({
      host: self.host,
      port: self.port,
      path: '/v1/agent/service/' + requestPath,
      method: 'PUT'
    });

    request.write(JSON.stringify(serviceData));
    request.end();

    return request;
  }

  function registerResourcesBase(serviceData, resourceArray, mode) {
    var requests = [];

    // Customize each request parameters
    resourceArray.forEach(function(resource) {
      var path = '/v1/kv/resources' + resource;
      var request = http.request({
        host: self.host,
        port: self.port,
        path: path,
        method: mode
      });

      requests.push(request);
    });

    // Start sending requests
    requests.forEach(function(request) {
      request.write(serviceData.Name);
      request.end();
    });

    return requests;
  }
}
function RegistrationService(host, port) {
  var self = this;

  self.consulHost = host;
  self.consulPort = port;
  self.filename = '.consul_service_id';

  self.registrator = new ServiceConsulRegistrator(self.consulHost, self.consulPort);;
  self.parser = new ServiceRAMLParser();

  self.register = function(args) {
    registration.registerService(args.name);

    if (args.raml) {
      registration.registerRaml(args.name, args.raml);
    }

    if (args.path) {
      registration.registerPath(args.name, args.path);
    }
  }

  self.registerService = function(serviceName) {
    return serviceRegistrationBase(serviceName, 'register');
  }

  self.registerRaml = function(serviceName, filename) {
    return ramlRegistrationBase(serviceName, filename, 'register');
  }

  self.registerPath = function(serviceName, paths) {
    return pathRegistrationBase(serviceName, paths, 'register');
  }

  self.deregister = function(args) {
    registration.deregisterService(args.name);

    if (args.raml) {
      registration.deregisterRaml(args.name, args.raml);
    }

    if (args.path) {
      registration.deregisterPath(args.name, args.path);
    }

    clearServiceId();
  }

  self.deregisterService = function(serviceName) {
    return serviceRegistrationBase(serviceName, 'deregister');
  }

  self.deregisterRaml = function(serviceName, filename) {
    return ramlRegistrationBase(serviceName, filename, 'deregister');
  }

  self.deregisterPath = function(serviceName, paths) {
    return pathRegistrationBase(serviceName, paths, 'deregister');
  }

  function getServiceData(serviceName) {
    var serviceId = getServiceId(serviceName);
    return {
      'ID': serviceId,
      'Name': serviceName,
      'Port': 8080,
      'Address': ip.address(),
      'Tags': ['openlmis-service'],
      'EnableTagOverride': false
    };
  }

  function getServiceId(serviceName) {
    var serviceId = null;

    try {
      fs.accessSync(self.filename, fs.R_OK | fs.W_OK);
      serviceId = fs.readFileSync(fs.openSync(self.filename, 'r+')).toString();
    } catch (err) {
      serviceId = uuid() + '-' + serviceName;
      fs.writeSync(fs.openSync(self.filename, 'w+'), serviceId);
    }

    return serviceId;
  }

  function clearServiceId() {
    try {
      fs.unlinkSync(self.filename);
    } catch (err) {
      console.error("Service ID file could not be found or accessed.");
    }

  }

  function serviceRegistrationBase(serviceName, mode) {
    var serviceData = getServiceData(serviceName, mode);
    if (mode === 'register') {
      return self.registrator.registerService(serviceData);
    } else {
      return self.registrator.deregisterService(serviceData);
    }
  }

  function ramlRegistrationBase(serviceName, filename, mode) {
    var serviceData = getServiceData(serviceName, mode);
    var resourceRequests;

    self.parser.extractResources(filename).then(function(resources) {
      if (mode === 'register') {
        resourceRequests = self.registrator.registerResources(serviceData, resources);
      } else {
        resourceRequests = self.registrator.deregisterResources(serviceData, resources);
      }
    });

    // Wait for RAML parsing
    while(!resourceRequests) {
      deasync.runLoopOnce();
    }

    return Promise.all(resourceRequests);
  }

  function pathRegistrationBase(serviceName, paths, mode) {
    var serviceData = getServiceData(serviceName, mode);

    for (var i = 0; i < paths.length; i++) {
      if (!paths[i].startsWith("/")) {
        paths[i] = "/" + paths[i];
      }
    }

    if (mode === 'register') {
      return self.registrator.registerResources(serviceData, paths);
    } else {
      return self.registrator.deregisterResources(serviceData, paths);
    }
  }
}

function processArgs() {
  var args = commandLineArgs([
    { name: 'config-file', alias: 'f', type: String },
    { name: 'name', alias: 'n', type: String },
    { name: 'command', alias: 'c', type: String },
    { name: 'raml', alias: 'r', type: String },
    { name: 'path', alias: 'p', type: String, multiple: true }
  ]);

  if (args['config-file']) {
    // If other arguments are not present, override them with config file values
    var config = JSON.parse(fs.readFileSync(fs.openSync(args['config-file'], 'r+')).toString());
    var values = ['name', 'command', 'path', 'raml'];

    for (var i = 0; i < values.length; i++) {
      var keyword = values[i];
      if (!(args[keyword])) {
        args[keyword] = config[keyword];
      }
    }
  }

  if (!args.name) {
    throw new Error("Name parameter is missing.");
  } else if (!args.command) {
    throw new Error("Command parameter is missing.");
  } else if (!(args.raml || args.path)) {
    throw new Error("You must either provide path or file parameter.");
  }

  return args;
}

try {
  var consulHost = process.env.CONSUL_HOST || 'consul';
  var consulPort = process.env.CONSUL_PORT || '8500';

  // Retrieve arguments passed to script
  var args = processArgs();

  var registration = new RegistrationService(consulHost, consulPort);
  var initialMessage = "Starting service " + args.command + "...";

  if (args.command === 'register') {
    console.log(initialMessage);
    registration.register(args);
  } else if (args.command === 'deregister') {
    console.log(initialMessage);
    registration.deregister(args);
  } else {
    throw new Error("Invalid command. It should be either 'register' or 'deregister'.")
  }

  console.log("Service " + args.command + " successful!");
} catch(err) {
  console.error("Error during service registration:")
  console.error(err.message);
  process.exit(1);
}
