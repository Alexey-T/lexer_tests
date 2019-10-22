    -- COMMENT LINE
    -- SQL samples - every single sql ends with <;>
    -- 

    SET client_encoding = 'SQL_ASCII';
    SET check_function_bodies = false;

    CREATE TRUSTED PROCEDURAL LANGUAGE plpgsql HANDLER plpgsql_call_handler VALIDATOR plpgsql_validator;

    CREATE FUNCTION pg_file_length(text) RETURNS bigint
        AS $_$SELECT len FROM pg_file_stat($1) AS s(len int8, c timestamp, a timestamp, m timestamp, i bool)$_$
        LANGUAGE sql STRICT;

    CREATE TABLE concedii (
        id serial NOT NULL,
        id_ang integer NOT NULL,
        tip character varying(2) NOT NULL,
        cod_cm character varying(2),
        de_la date NOT NULL,
        pana_la date NOT NULL,
        suma double precision NOT NULL,
        dup character(1),
        zile integer,
        tipcm character(1),
        data_avans_co date,
        procent_cm double precision,
        zile_cm_unitate integer,
        zile_cm_fond integer,
        venituri_baza_cm double precision,
        serie_cm character varying(5),
        numar_cm character varying(10),
        zile_baza_cm integer,
        cnp_copil character varying(13),
        mzbc double precision,
        data_certificat date,
        loc_prescriere_cm character varying(2),
        id_cm_initial double precision
    );

    COMMENT ON COLUMN concedii.data_certificat IS 'data eliberarii certificatului medical';

    CREATE INDEX date_lunare_idx ON date_lunare USING btree (id, perioada);


    --
    -- some additional sql just for testing
    --

    SELECT DISTINCT categoria FROM config WHERE modul='configurare' ORDER BY categoria;

    DELETE FROM testtable where id>0;

    DROP TABLE testtable;

    -- this is one single select with union
    SELECT * FROM persoane WHERE id IN
       (select id from date_lunare where perioada>=100 and perioada<=200) order by id
    UNION ALL
    SELECT * FROM persoane2 where id in
       (select id from date_lunare where perioada>=100 and perioada<=200) order by id;
