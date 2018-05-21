# OpenLMIS Requisition Service
This repository holds the files for the OpenLMIS Requisition Independent Service.

## Prerequisites
* Docker 1.11+
* Docker Compose 1.6+

## <a name="quickstart">Quick Start</a>
1. Fork/clone this repository from GitHub.

 ```shell
 git clone https://github.com/OpenLMIS/openlmis-requisition.git
 ```
2. Add an environment file called `.env` to the root folder of the project, with the required 
project settings and credentials. For a starter environment file, you can use [this 
one](https://raw.githubusercontent.com/OpenLMIS/openlmis-ref-distro/master/settings-sample.env). e.g.

 ```shell
 cd openlmis-requisition
 curl -o .env -L https://raw.githubusercontent.com/OpenLMIS/openlmis-ref-distro/master/settings-sample.env
 ```
3. Develop w/ Docker by running `docker-compose run --service-ports requisition`.
See [Developing w/ Docker](#devdocker). You should now be in an interactive shell inside
the newly created development environment.
4. Run `gradle build` to build. After the build steps finish, you should see 'Build Successful'.
5. Start the service with `gradle bootRun`. Once it is running, you should see
'Started Application in NN seconds'. Your console will not return to a prompt as long as
the service is running. The service may write errors and other output to your console.
6. You must authenticate to get a valid `access_token` before you can use the service.
Follow the [Security](https://github.com/OpenLMIS/openlmis-example/blob/master/README.md#security)
instructions to generate a POST request to the authorization server at `http://localhost:8081/`.
You can use a tool like [Postman](https://www.getpostman.com/) to generate the POST.
The authorization server will return an `access_token` which you must save for use on requests to
this OpenLMIS service. The token will expire with age, so be ready to do this step often.
7. Go to `http://localhost:8080/?access_token=<yourAccessToken>` to see the service name and version.
Note: If localhost does not work, the docker container with the service running might not be
bridged to your host workstation. In that case, you can determine your Docker IP address by
running `docker-machine ip` and then visit `http://<yourDockerIPAddress>:8080/`.
8. Go to `http://localhost:8080/index.html?access_token=<yourAccessToken>` to see the Swagger UI showing the API endpoints.
(Click 'default' to expand the list.)
9. Use URLs of the form `http://localhost:8080/api/*?access_token=<yourAccessToken>` to hit
the APIs directly.

## Stopping the Service
To stop the service (when it is running with `gradle bootRun`) use Control-C.

To clean up unwanted Docker containers, see the [Docker Cheat Sheet](https://openlmis.atlassian.net/wiki/display/OP/Docker+Cheat+Sheet#DockerCheatSheet-Cleaningup:).

## API Definition and Testing
See the API Definition and Testing section in the Example Service README at
https://github.com/OpenLMIS/openlmis-example/blob/master/README.md#api.

## Building & Testing
See the Building & Testing section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#building.

## Security
See the Security section in the Example Service README at
https://github.com/OpenLMIS/openlmis-example/blob/master/README.md#security.

## <a name="devdocker">Developing with Docker</a>
See the Developing with Docker section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#devdocker.

### Development Environment
See the Development Environment section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#devenv.

### Build Deployment Image
See the Build Deployment Image section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#buildimage.

### Publish to Docker Repository
TODO

### Docker's file details
See the Docker's file details section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#dockerfiles.

### Running complete application with nginx proxy
See the Running complete application with nginx proxy section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#nginx.

### Logging
See the Logging section in the Service Template README at 
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#logging.

### Internationalization (i18n)
See the Internationalization section in the Service Template README at 
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#internationalization.

### Debugging
See the Debugging section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#debugging.

### Demo Data
A basic set of demo data is included with this service, defined under `./demo-data/`.  This data may
be optionally loaded by using the `demo-data` Spring Profile.  Setting this profile may be done by
setting the `spring.profiles.active` environment variable.

When building locally from the development environment, you may run:

```shell
$ export spring_profiles_active=demo-data
$ gradle bootRun
```

To see how to set environment variables through Docker Compose, see the
[Reference Distribution](https://github.com/openlmis/openlmis-ref-distro)

### Performance Testing Data
A generated, sufficiently-large set of data is defined in 
`./src/main/resources/db/performance-data/`.  This data set is meant for performance testing, which
helps answer questions such as:

1. what happens to this operation when there are 10,000 Orderables / users / facilities?
1. how many concurrent requests can we serve?
1. what's the network characteristic for clients on mobile connections?

This data set builds upon the demo data set and therefore must be loaded after demo-data is loaded.
To do so activate the `performance-data` Spring Profle after demo-data, for example:

```shell
$ export spring_profiles_active=demo-data,performance-data
$ gradle bootRun
```

### Refresh DB

If the service is being deployed against a database which has data, and that data has been modified
outside of the operation of the Service (or sometimes when upgrading this Service), you'll want
to run the Service at least once with the following profile:

```
spring_profiles_active=refresh-db
```

This will ensure that the database works well with this Service, including:
* Run the audit log initializer which ensures everything in the database has an audit log.

## Production by Spring Profile

By default when this service is started, it will clean its schema in the database before migrating
it. This is meant for use during the normal development cycle. For production data, this obviously
is not desired as it would remove all of the production data. To change the default clean & migrate
behavior to just be a migrate behavior (which is still desired for production use), we use a Spring
Profile named `production`. To use this profile, it must be marked as Active. The easiest way to
do so is to add to the .env file:

```java
spring_profiles_active=production
```

This will set the similarly named environment variable and limit the profile in use.  The
expected use-case for this is when this service is deployed through the
[Reference Distribution](https://github.com/openlmis/openlmis-ref-distro).

## Database Clustering on a Schedule

In order to improve performance on some database tables, a scheduled task has 
been added to this Service, which will perform clustering on those tables. In 
order to use this, set the following environment variables:

* **DB_CLUSTERING_ENABLED** - Boolean that determines if the clustering tasks 
should be run. Set to true or false; false if not set.

* **DB_CLUSTERING_CRON_EXP** - A cron expression that determines on what 
schedule the clustering tasks are run. See the Spring [documentation](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/scheduling/support/CronSequenceGenerator.html) to know the format.


## Environment variables

Environment variables common to all services are listed here: https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md#environment-variables

* **CONSUMED_REASON_ID** - the ID of the reason that will be used to make consumption entries in Stock Management physical inventory

* **RECEIPTS_REASON_ID** - the ID of the reason that will be used to make receipts entries in Stock Management physical inventory
