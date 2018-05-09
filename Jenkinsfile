pipeline {
    agent any
    options {
        buildDiscarder(logRotator(numToKeepStr: '15'))
    }
    environment {
      PATH = "/usr/local/bin/:$PATH"
    }
    parameters {
        text(defaultValue: "", description: 'Custom environment variables to be used in contract tests', name: 'customEnv')
    }
    stages {
        stage('Preparation') {
            steps {
                checkout scm

                withCredentials([usernamePassword(
                  credentialsId: "cad2f741-7b1e-4ddd-b5ca-2959d40f62c2",
                  usernameVariable: "USER",
                  passwordVariable: "PASS"
                )]) {
                    sh 'set +x'
                    sh 'docker login -u $USER -p $PASS'
                }
                script {
                    def properties = readProperties file: 'gradle.properties'
                    if (!properties.serviceVersion) {
                        error("serviceVersion property not found")
                    }
                    VERSION = properties.serviceVersion
                    VERSION_WITH_BUILD_NUMBER = properties.serviceVersion + "-build" + env.BUILD_NUMBER
                    currentBuild.displayName += " - " + VERSION
                }
            }
        }
        stage('Build') {
            steps {
                withCredentials([file(credentialsId: '8da5ba56-8ebb-4a6a-bdb5-43c9d0efb120', variable: 'ENV_FILE')]) {
                    sh 'set +x'
                    sh 'sudo rm -f .env'
                    sh 'cp $ENV_FILE .env'

                    sh 'docker-compose -f docker-compose.builder.yml run -e BUILD_NUMBER=$BUILD_NUMBER -e GIT_BRANCH=$GIT_BRANCH builder'
                    sh 'docker-compose -f docker-compose.builder.yml build image'
                    sh 'docker-compose -f docker-compose.builder.yml down --volumes'
                    sh "docker tag openlmis/requisition:latest openlmis/requisition:${VERSION_WITH_BUILD_NUMBER}"
                }
            }
            post {
                success {
                    archive 'build/libs/*.jar,build/resources/main/api-definition.html, build/resources/main/  version.properties'
                }
                always {
                    checkstyle pattern: '**/build/reports/checkstyle/*.xml'
                    pmd pattern: '**/build/reports/pmd/*.xml'
                    junit '**/build/test-results/*/*.xml'
                }
            }
        }
        stage('Parallel: Sonar analysis and contract tests') {
            parallel {
                stage('Sonar analysis') {
                    steps {
                        withSonarQubeEnv('Sonar OpenLMIS') {
                            withCredentials([string(credentialsId: 'SONAR_LOGIN', variable: 'SONAR_LOGIN'), string(credentialsId: 'SONAR_PASSWORD', variable: 'SONAR_PASSWORD')]) {
                                sh '''
                                    set +x
                                    sudo rm -f .env

                                    curl -o .env -L https://raw.githubusercontent.com/OpenLMIS/openlmis-ref-distro/master/settings-sample.env
                                    sed -i '' -e "s#spring_profiles_active=.*#spring_profiles_active=#" .env  2>/dev/null || true
                                    sed -i '' -e "s#^BASE_URL=.*#BASE_URL=http://localhost#" .env  2>/dev/null || true
                                    sed -i '' -e "s#^VIRTUAL_HOST=.*#VIRTUAL_HOST=localhost#" .env  2>/dev/null || true

                                    docker-compose -f docker-compose.builder.yml run sonar
                                    docker-compose -f docker-compose.builder.yml down --volumes
                                '''
                                // workaround: Sonar plugin retrieves the path directly from the output
                                sh 'echo "Working dir: ${WORKSPACE}/build/sonar"'
                            }
                        }
                        timeout(time: 1, unit: 'HOURS') {
                            script {
                                def gate = waitForQualityGate()
                                if (gate.status != 'OK') {
                                    error 'Quality Gate FAILED'
                                }
                            }
                        }
                    }
                }
                stage('Contract tests') {
                    steps {
                        build job: 'OpenLMIS-requisition-contract-test', propagate: true, wait: true
                        build job: 'OpenLMIS-fulfillment-contract-test', propagate: true, wait: true
                    }
                }
            }
        }
        stage('ERD generation') {
            steps {
                dir('erd') {
                    sh '''#!/bin/bash -xe
                        docker pull openlmis/requisition
                        #the image might be built on the jenkins slave, so we need to pull here to make sure it's using the latest

                        # prepare ERD folder on CI server
                        sudo mkdir -p /var/www/html/erd-requisition
                        sudo chown -R $USER:$USER /var/www/html/erd-requisition

                        # General steps:
                        # - Copy env file and remove demo data profiles (errors happen during startup when they are enabled)
                        # - Copy ERD generation docker-compose file and bring up service with db container and wait
                        # - Clean out existing ERD folder
                        # - Create output folder (SchemaSpy uses it to hold ERD files) and make sure it is writable by docker
                        # - Use SchemaSpy docker image to generate ERD files and send to output, wait
                        # - Bring down service and db container
                        # - Make sure output folder and its subfolders is owned by user (docker generated files/folders are owned by docker)
                        # - Move output to web folder
                        # - Clean out old zip file and re-generate it
                        # - Clean up files and folders
                        wget https://raw.githubusercontent.com/OpenLMIS/openlmis-ref-distro/master/settings-sample.env -O .env \
                        && sed -i -e "s/^spring_profiles_active=demo-data,refresh-db/spring_profiles_active=/" .env \
                        && wget https://raw.githubusercontent.com/OpenLMIS/openlmis-requisition/master/docker-compose.erd-generation.yml -O docker-compose.yml \
                        && (/usr/local/bin/docker-compose up &) \
                        && sleep 90 \
                        && sudo rm /var/www/html/erd-requisition/* -rf \
                        && rm -rf output \
                        && mkdir output \
                        && chmod 777 output \
                        && (docker run --rm --network erd_default -v $WORKSPACE/erd/output:/output schemaspy/schemaspy:snapshot -t pgsql -host db -port 5432 -db open_lmis -s requisition -u postgres -p p@ssw0rd -I "(data_loaded)|(schema_version)|(jv_.*)" -norows -hq &) \
                        && sleep 30 \
                        && /usr/local/bin/docker-compose down --volumes \
                        && sudo chown -R $USER:$USER output \
                        && mv output/* /var/www/html/erd-requisition \
                        && rm erd-requisition.zip -f \
                        && pushd /var/www/html/erd-requisition \
                        && zip -r $WORKSPACE/erd/erd-requisition.zip . \
                        && popd \
                        && rmdir output \
                        && rm .env \
                        && rm docker-compose.yml
                    '''
                    archiveArtifacts artifacts: 'erd-requisition.zip'
                }
            }
        }
        stage('Push image') {
            when {
                expression {
                    return env.GIT_BRANCH == 'master' || env.GIT_BRANCH =~ /rel-.+/
                }
            }
            steps {
                sh "docker tag openlmis/requisition:${VERSION_WITH_BUILD_NUMBER} openlmis/requisition:${VERSION}"
                sh "docker push openlmis/requisition:${VERSION}"
            }
        }
    }
    post {
        failure {
            slackSend color: 'danger', message: '${env.JOB_NAME} - ${env.BUILD_NUMBER} FAILED (<${env.BUILD_URL}|Open>)'
        }
    }
}
