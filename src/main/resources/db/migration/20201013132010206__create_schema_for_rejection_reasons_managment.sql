create TABLE IF NOT EXISTS rejection_reason_categories
(
  id uuid NOT NULL,
  active boolean DEFAULT true,
  name text NOT NULL,
  code text NOT NULL,
  CONSTRAINT rejection_reason_categories_pkey PRIMARY KEY (id)
);


create TABLE IF NOT EXISTS rejection_reasons
(
  id uuid NOT NULL,
  active boolean DEFAULT true,
  name text NOT NULL,
  code text NOT NULL,
  rejectionreasoncategoryid uuid NOT NULL,
  CONSTRAINT rejection_reasons_pkey PRIMARY KEY (id)
);



alter table rejection_reasons
    add CONSTRAINT fk_rejection_reason_categories FOREIGN KEY (rejectionreasoncategoryid) REFERENCES rejection_reason_categories (id);

    alter table ONLY rejection_reason_categories ADD CONSTRAINT unique_rejection_reason_categories UNIQUE (code);
    alter table ONLY rejection_reason_categories ADD CONSTRAINT name_rejection_reason_categories UNIQUE (name);
    alter table ONLY rejection_reasons ADD CONSTRAINT unique_rejection_reasons UNIQUE (code);
    alter table ONLY rejection_reasons ADD CONSTRAINT name_rejection_reasons UNIQUE (name);



