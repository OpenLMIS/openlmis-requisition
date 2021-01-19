CREATE TABLE rejections (
    id uuid NOT NULL,
    createddate timestamp with time zone,
    modifieddate timestamp with time zone,
    rejectionreasonid uuid NOT NULL,
    statuschangeid uuid,
    CONSTRAINT rejections_pkey PRIMARY KEY (id)
);


alter table rejections
    add CONSTRAINT fk_status_changes FOREIGN KEY (statuschangeid) REFERENCES status_changes (id);
alter table rejections
    add CONSTRAINT fk_rejection_reasons FOREIGN KEY (rejectionreasonid) REFERENCES rejection_reasons (id);

