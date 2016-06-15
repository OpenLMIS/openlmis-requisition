# OpenLMIS Requisition Service
This repository holds the files for the OpenLMIS Requisition Independent Service.

## Prerequisites
* Docker 1.11+
* Docker Compose 1.6+

## Quick Start

1. Fork/clone this repository from GitHub.

 ```shell
 git clone https://github.com/OpenLMIS/openlmis-requisition.git
 ```
2. Add an environment file called `.env` to the root folder of the project, with the required 
project settings and credentials. For a starter environment file, you can use [this 
one](https://github.com/OpenLMIS/openlmis-config/blob/master/.env).
3. Develop w/ Docker by running `docker-compose run --service-ports requisition`.
See [Developing w/ Docker](#devdocker).
4. You should now be in an interactive shell inside the newly created development 
environment, start the Service with: `gradle bootRun`
5. Go to `http://<yourDockerIPAddress>:8080/` to see the service name 
and version. Note that you can determine yourDockerIPAddress by running `docker-machine ip`.
6. Go to `http://<yourDockerIPAddress>:8080/api/` to see the APIs.

## Building & Testing

Gradle is our usual build tool.  This template includes common tasks 
that most Services will find useful:

- `clean` to remove build artifacts
- `build` to build all source. `build`, after building sources, also starts unit tests. Build will be successful only if all tests pass.
- `generateMigration -PmigrationName=<yourMigrationName>` to create a
"blank" database migration file. The file
will be generated under `src/main/resources/db/migration`. Put your
migration SQL into it.
- `test` to run unit tests
- `integrationTest` to run integration tests
- `sonarqube` to execute the SonarQube analysis.

The **test results** are shown in the console.

While Gradle is our usual build tool, OpenLMIS v3+ is a collection of
Independent Services where each Gradle build produces 1 Service.
To help work with these Services, we use Docker to develop, build and
publish these.

See [Developing with Docker](#devdocker).

##<a name="devdocker"></a> Developing with Docker

OpenLMIS utilizes Docker to help with development, building, publishing
and deployment of OpenLMIS Services. This helps keep development to
deployment environments clean, consistent and reproducible and
therefore using Docker is recommended for all OpenLMIS projects.

To enable development in Docker, OpenLMIS publishes a couple Docker
Images:

- [openlmis/dev](https://hub.docker.com/r/openlmis/dev/) - for Service
development.  Includes the JDK & Gradle plus common build tools.
- [openlmis/postgres](https://hub.docker.com/r/openlmis/postgres/) - for
quickly standing up a shared PostgreSQL DB

In addition to these Images, each Service includes Docker Compose
instructions to:

- standup a development environment (run Gradle)
- build a lean image of itself suitable for deployment
- publish its deployment image to a Docker Repository

### Development Environment
Launches into shell with Gradle & JDK available suitable for building
Service.  PostgreSQL connected suitable for testing. If you run the
Service, it should be available on port 8080.

Before starting the development environment, make sure you have a `.env` file as outlined in the 
Quick Start instructions.

```shell
> docker-compose run --service-ports requisition
$ gradle clean build
$ gradle bootRun
```

### Build Deployment Image
The specialized docker-compose.builder.yml is geared toward CI and build
servers for automated building, testing and docker image generation of
the service.

Before building the deployment image, make sure you have a `.env` file as outlined in the Quick
Start instructions.

```shell
> docker-compose -f docker-compose.builder.yml run builder
> docker-compose -f docker-compose.builder.yml build image
```

### Publish to Docker Repository
TODO

### Docker's file details
A brief overview of the purpose behind each docker related file

- `Dockerfile`:  build a deployment ready image of this service
suitable for publishing.
- `docker-compose.yml`:  base docker-compose file.  Defines the
basic composition from the perspective of working on this singular
vertical service.  These aren't expected to be used in the
composition of the Reference Distribution.
- `docker-compose.override.yml`:  extends the `docker-compose.yml`
base definition to provide for the normal usage of docker-compose
inside of a single Service:  building a development environment.
Wires this Service together with a DB for testing, a gradle cache
volume and maps tomcat's port directly to the host.
- `docker-compose.builder.yml`:  an alternative docker-compose file
suitable for CI type of environments to test & build this Service
and generate a publishable/deployment ready Image of the service.

### Logging
See the Logging section in the Service Template README at 
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md.

### Internationalization (i18n)
See the Internationalization section in the Service Template README at 
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md.

### Debugging
See the Debugging section in the Service Template README at
https://github.com/OpenLMIS/openlmis-template-service/blob/master/README.md.
