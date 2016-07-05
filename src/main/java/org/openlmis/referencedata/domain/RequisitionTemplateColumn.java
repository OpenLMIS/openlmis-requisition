package org.openlmis.referencedata.domain;

import javax.persistence.Embeddable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Embeddable
@NoArgsConstructor
@Getter
@Setter
public class RequisitionTemplateColumn{

    String name;

    String label;

    int displayOrder;

    public RequisitionTemplateColumn(String name, String label, int displayOrder) {
        this.name = name;
        this.label = label;
        this.displayOrder = displayOrder;
    }

}
