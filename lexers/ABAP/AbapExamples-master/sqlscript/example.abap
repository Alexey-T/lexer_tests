class zcl_bryan_amdp_test definition public final create public .
public section.
    interfaces: if_amdp_marker_hdb.

    types:
        begin of totals,
            total type float,
        end of totals,
        total_table type table of totals.

    methods: amdp_execute
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year) type gjahr
                  value(in_period) type i
        exporting value(out_previous) type f
                  value(out_period) type f.

protected section.
private section.

    methods: _amdp_previous_years
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year) type gjahr
        exporting value(out_total) type f.

    methods: _amdp_period_1
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_2
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_3
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_4
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_5
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_6
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_7
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_8
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_9
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_10
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_11
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_12
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_13
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_14
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_15
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.

    methods: _amdp_period_16
        importing value(mandt) type mandt
                  value(in_object) type j_objnr
                  value(in_year)   type gjahr
        exporting value(out_period) type f.


endclass.

class zcl_bryan_amdp_test implementation.

    method amdp_execute by database procedure for hdb
        language sqlscript options
        read-only using ZCL_BRYAN_AMDP_TEST=>_AMDP_PREVIOUS_YEARS
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_1
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_2
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_3
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_4
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_5
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_6
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_7
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_8
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_9
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_10
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_11
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_12
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_13
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_14
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_15
                        ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_16.

        declare previous_total float;
        declare period_total float;

        -- Step 1, get the previous years
        call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PREVIOUS_YEARS" (
            mandt     => :mandt,
            in_object => :in_object,
            in_year   => :in_year,
            out_total => :previous_total );

        -- Step 2, determine period and call appropriate stored procedure
        if ( :in_period = 1 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_1" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 2 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_2" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 3 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_3" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 4 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_4" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 5 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_5" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 6 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_6" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 7 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_7" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 8 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_8" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 9 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_9" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 10 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_10" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 11 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_11" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 12 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_12" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 13 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_13" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 14 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_14" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 15 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_15" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        elseif ( :in_period = 16 ) then
            call "ZCL_BRYAN_AMDP_TEST=>_AMDP_PERIOD_16" (
                mandt => :mandt,
                in_object => :in_object,
                in_year => :in_year,
                out_period => :period_total );
        end if;

        out_previous := previous_total;
        out_period   := period_total;

    endmethod.

    "-- Private Methods --"

    method _amdp_previous_years  by database procedure for hdb
        language sqlscript options read-only
        using rpsco.
        declare totals double array;
        declare found int;
        declare value double;
        results = select 
            ifnull ( 
                to_double( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
                           sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) +
                           sum( wlp11 ) + sum( wlp12 ) + sum( wlp13 ) + sum( wlp14 ) + sum( wlp15 ) +
                           sum( wlp16 ) ), 0.0 ) as total from rpsco
                   where objnr = :in_object
                     and gjahr < :in_year;
        totals := array_agg( :results.total );
        out_total := :totals[1];
    endmethod.

    method _amdp_period_1 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select 
            ifnull ( 
                to_double( sum( wlp01 ) ), 0.0 ) as total from rpsco
                   where objnr = :in_object
                     and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_2 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select 
            ifnull ( 
                to_double ( sum( wlp01 ) + sum ( wlp02 ) ), 0.0 ) as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_3 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select 
            ifnull ( 
                to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_4 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select 
            ifnull ( 
                to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_5 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_6 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_7 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_8 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_9 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_10 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_11 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) +
               sum( wlp11 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_12 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) +
               sum( wlp11 ) + sum( wlp12 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_13 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) +
               sum( wlp11 ) + sum( wlp12 ) + sum( wlp13 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_14 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) +
               sum( wlp11 ) + sum( wlp12 ) + sum( wlp13 ) + sum( wlp14 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_15 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) +
               sum( wlp11 ) + sum( wlp12 ) + sum( wlp13 ) + sum( wlp14 ) + sum( wlp15 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

    method _amdp_period_16 by database procedure for hdb
        language sqlscript
        options read-only
        using rpsco.
        declare totals double array;
        results = select ifnull ( to_double ( sum( wlp01 ) + sum( wlp02 ) + sum( wlp03 ) + sum( wlp04 ) + sum( wlp05 ) +
               sum( wlp06 ) + sum( wlp07 ) + sum( wlp08 ) + sum( wlp09 ) + sum( wlp10 ) +
               sum( wlp11 ) + sum( wlp12 ) + sum( wlp13 ) + sum( wlp14 ) + sum( wlp15 ) + sum( wlp16 ) ), 0.0 )
            as total from rpsco
            where objnr = :in_object
              and gjahr = :in_year;
        totals := array_agg( :results.total );
        out_period := :totals[1];
    endmethod.

endclass.

