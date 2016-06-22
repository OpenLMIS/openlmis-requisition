package org.openlmis.referencedata.validate;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import java.time.LocalDate;
import java.util.UUID;
import javax.validation.Constraint;
import javax.validation.Payload;
import org.openlmis.referencedata.domain.Schedule;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Documented
@Constraint(validatedBy = DateConstraintValidator.class)
@Target( { TYPE, METHOD, ANNOTATION_TYPE })
@Retention(RUNTIME)
public @interface DateValidator {

    String message() default "invalid.date.message";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}