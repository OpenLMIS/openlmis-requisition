package org.openlmis.utils;

import javax.persistence.AttributeConverter;
import javax.persistence.Converter;

import java.sql.Date;
import java.time.LocalDate;

@Converter
public class LocalDatePersistenceConverter implements AttributeConverter<LocalDate, Date> {

    @Override
    public Date convertToDatabaseColumn(LocalDate entityValue) {
        if (entityValue != null) {
            return Date.valueOf(entityValue);
        }
        return null;
    }

    @Override
    public LocalDate convertToEntityAttribute(Date databaseValue) {
        if (databaseValue != null) {
            return databaseValue.toLocalDate();
        }
        return null;
    }
}