FROM openlmis/service-base

COPY build/libs/*.jar /service.jar
COPY build/demo-data /demo-data
COPY build/consul /consul
