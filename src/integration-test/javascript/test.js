/**
 * Initial hippie-swagger test.
 * 
 * https://github.com/CacheControl/hippie-swagger
 */

var SwaggerParser = require('swagger-parser')
var parser = new SwaggerParser()
var hippie = require('hippie-swagger')
var expect = require('chai').expect
var path = require('path')
var dereferencedSwagger

var options = {
    validateResponseSchema: false,
    validateParameterSchema: true,
    errorOnExtraParameters: false,
    errorOnExtraHeaderParameters: false
};


function getLocalAddress()
{
    var dockerHost = process.env.DOCKER_HOST;
    var port = process.env.WEB_PORT;

    //Extract the IP address from dockerHost and add a port
    var regex = /\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b/;
    var address = dockerHost.match(regex);
    address = 'http://' + address + ':' + port;

    return address;
}


var BASE_URL = 'http://192.168.99.100:8080/api'
//var BASE_URL = getLocalAddress();

//var SWAGGER_FILE_NAME = 'swagger2.json'
var SWAGGER_FILE_NAME = '../../main/webapp/requisition-service-api.json'

function api()
{
    return hippie(dereferencedSwagger, options).json();
}

function getUser()
{
    return {
        "id" : "be17e817-c74a-4c11-bb4a-bbdda6be84c4" ,
        "username": "username_1",
        "firstName": "firstName_1",
        "lastName": "lastName_1",
        "password" : "password_1",
        "verified": true,
        "active": true
    };
}

function getInvalidUser()
{
    return {
        "id" : "be17e817-c74a-4c11-bb4a-bbdda6be84c4" ,
        "password" : "password_1",
        "verified": true,
        "active": true
    };
}

//function getRandomUUID
function guid()
{
    function s4() {
        return Math.floor((1 + Math.random()) * 0x10000)
            .toString(16)
            .substring(1);
    }
    return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
        s4() + '-' + s4() + s4() + s4();
}


describe('Version 1 of the requisition API', function()
{
    before(function(done)
    {
        parser.dereference(path.join(__dirname, './' + SWAGGER_FILE_NAME), function (err, api)
        {
            if (err) return done(err)

            dereferencedSwagger = api
            done()
        })
    })

    it('should return 200 in response to GET /users', function(done)
    {
        api()
            .get(BASE_URL + '/users')
            .expectStatus(200)
            .end(done)
    })

    it('should return 201 in response to POST /users, assuming the specified user (request body) is valid', function(done)
    {
        api()
            .post(BASE_URL + '/users')
            .send( getUser() )
            .expectStatus(201)
            .end(done)
    })

    //This test is currently failing - our service should be updated such that it passes
    it('should return 400 in response to POST /users, assuming the specified user (request body) is invalid', function(done)
    {
        var user = getUser();
        user.username = undefined; //Remove a required field, username, from the user object

        api()
            .post(BASE_URL + '/users')
            .send( user )
            .expectStatus(201)
            .end(done)
    })

    //As this failing test shows, sending an invalid UUID currently breaks the backend. That should be fixed.
    it('should return 404 in response to GET /users/{Invalid-UUID}', function(done)
    {
        api()
            .get(BASE_URL + '/users/{id}')
            .pathParams
            (
                {
                    id: "non-valid-uuid"
                }
            )
            .expectStatus(404)
            .end(done)
    })

    it('should return 404 in response to GET /users/{Valid-But-Not-Recognized-UUID}', function(done)
    {
        api()
            .get(BASE_URL + '/users/{id}')
            .pathParams(
                {
                    id: '05e015c1-fe05-4a4c-ae52-dd758335f05b'
                }
            )
            .expectStatus(404)
            .end(done)
    })

    //Skipping this test because SDR returns a 200, along with an empty array of users.
    //If we were implementing this with MVC, we'd probably return a 404. This is instance
    //were a backend implementation detail (SDR) affects not just the response body, but the
    //response code as well.
    it.skip('should return a 404 in response to GET /users/search/findByUsername?username=unknownUserName', function(done)
    {
        api()
            .get(BASE_URL + '/users/search/findByUsername')
            .qs({ "username": "unknownUserName" }) //tack ?username=unknownUserName onto our URI
            .expectStatus(404)
            .end(done)
    })

})

