package org.openlmis.referencedata.domain;

import org.openlmis.referencedata.validate.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

@Entity
@Table(name = "periods")
@NoArgsConstructor
@DateValidator(start="startDate", end="endDate")
public class Period extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "processingScheduleId",  nullable = false)
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
    private LocalDate startDate;

    @Column(nullable = false)
    @Getter
    @Setter
    private LocalDate endDate;

}