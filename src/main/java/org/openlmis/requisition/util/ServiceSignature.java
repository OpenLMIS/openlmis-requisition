package org.openlmis.requisition.util;

import lombok.Getter;
import lombok.Setter;

public class ServiceSignature {

    public static final String SERVICE_NAME = "openlmis-requisition";
    public static final String SERVICE_VERSION = "0.0.1";

    @Getter
    @Setter
    private String name;

    @Getter
    @Setter
    private String version;
    
    public ServiceSignature(String name, String version) {
        this.name = name;
        this.version = version;
    }
}
