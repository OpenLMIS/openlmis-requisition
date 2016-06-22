package org.openlmis.referencedata.domain;

import org.openlmis.referencedata.validate.DateValidator;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import java.time.LocalDate;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;

@Entity
@Table(name = "periods")
@NoArgsConstructor
//@DateValidator
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

    public Set<ConstraintViolation<Period>> getValidationViolations() {
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();
            return validator.validate(this);
    }

    public boolean isValid() {
        return getValidationViolations().size() == 0;
    }
}