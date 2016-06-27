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

    // other
*/
}
