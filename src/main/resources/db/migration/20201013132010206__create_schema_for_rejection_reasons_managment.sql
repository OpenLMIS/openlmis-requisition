create TABLE IF NOT EXISTS rejection_reason_category
(
  id uuid NOT NULL,
  active boolean DEFAULT true NOT NULL,
  name text,
  code text NOT NULL,
  CONSTRAINT rejection_reason_category_pkey PRIMARY KEY (id)
);


create TABLE IF NOT EXISTS rejection_reason
(
  id uuid NOT NULL,
  active boolean DEFAULT true NOT NULL,
  name text,
  code text NOT NULL,
  rejectionreasoncategoryid uuid NOT NULL,
  CONSTRAINT rejections_pkey PRIMARY KEY (id)
);



alter table rejection_reason
    add CONSTRAINT fk_rejection_reason_category FOREIGN KEY (rejectionreasoncategoryid) REFERENCES rejection_reason_category (id);

    alter table ONLY rejection_reason_category ADD CONSTRAINT unique_rejection_reason_category UNIQUE (code);
    alter table ONLY rejection_reason_category ADD CONSTRAINT name_rejection_reason_category UNIQUE (name);
    alter table ONLY rejection_reason ADD CONSTRAINT unique_rejection_reason UNIQUE (code);
    alter table ONLY rejection_reason ADD CONSTRAINT name_rejection_reason UNIQUE (name);



