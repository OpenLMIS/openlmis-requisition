package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Table(name = "requisitionTemplates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseEntity {

    @OneToOne(cascade=CascadeType.ALL)
    @JoinColumn(name = "programid")
    @Getter
    @Setter
    private Program program;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String remarks;


/*
    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String quantityDispensed;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String beginningBalance;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String quantityReceived;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String quantityApproved;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String lossessAndAdjustments;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String stockOutDays;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String normalizedConsumption;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String quantityRequested;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String reasonForRequestedQuantity;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String newPatientCount;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String cost;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String price;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String total;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String product;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String dispensingUnit;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String productCode;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String packsToShip;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String skipped;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String calculatedOrderQuantity;

*/
}
