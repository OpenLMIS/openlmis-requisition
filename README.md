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
one](https://github.com/OpenLMIS/openlmis-config/blob/master/.env). e.g.

 ```shell
 cd openlmis-requisition
 curl -LO https://raw.githubusercontent.com/OpenLMIS/openlmis-config/master/.env
 ```
3. Develop w/ Docker by running `docker-compose run --service-ports requisition`.
See [Developing w/ Docker](#devdocker).
4. You should now be in an interactive shell inside the newly created development 
environment, start the Service with: `gradle bootRun`
5. Go to `http://<yourDockerIPAddress>:8080/` to see the service name 
and version. Note that you can determine yourDockerIPAddress by running `docker-machine ip`.
6. Go to `http://<yourDockerIPAddress>:8080/api/` to see the APIs.

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
You can use a standard data set for demonstration purposes. To do so, first follow the Quick Start
until step 3 is done: https://github.com/OpenLMIS/openlmis-requisition/blob/master/README.md#quickstart.
Then, before `gradle bootRun`, use `gradle demoDataSeed`. This will generate a sql input file under
`./demo-data` directory.

To insert this data into the database, finish the Quick Start steps,
and then outside of container's interactive shell, run:
`docker exec -i openlmisrequisition_db_1 psql -Upostgres open_lmis < demo-data/input.sql`

