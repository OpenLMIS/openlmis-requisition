package org.openlmis.referencedata.validate;

import java.lang.annotation.Documented;
import org.openlmis.referencedata.domain.Schedule;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.Payload;
import java.time.LocalDate;
import java.util.UUID;

@Documented
@Constraint(validatedBy = DateConstraintValidator.class)
@Target( { METHOD, FIELD })
@Retention(RUNTIME)
public @interface DateValidator {

    String message() default "{DateValidator}";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    String start();

    String end();
}