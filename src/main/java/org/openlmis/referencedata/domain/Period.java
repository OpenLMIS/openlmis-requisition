package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "period")
@NoArgsConstructor
public class Period extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "scheduleId", nullable = false)
    @Getter
    @Setter
    private Schedule processingSchedule;

    @Column(nullable = false, columnDefinition = "text")
    @Getter
    @Setter
    private String name;

    @Column(nullable = true, columnDefinition = "text")
    @Getter
    @Setter
    private String description;

    @Column(nullable = false)
    @Getter
    @Setter
    private Date startDate;

    @Column(nullable = false)
    @Getter
    @Setter
    private Date endDate;

}