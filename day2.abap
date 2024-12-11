FUNCTION znp_aoc_2024_day2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_PART1) TYPE  I
*"     REFERENCE(EV_PART2) TYPE  I
*"----------------------------------------------------------------------
  DATA: lv_file  TYPE string,
        lv_day   TYPE c VALUE 2,
        lt_lines TYPE STANDARD TABLE OF string,
        lv_line  LIKE LINE OF lt_lines.

* Read input
  CONCATENATE gc_filepath lv_day gc_txt INTO lv_file.
  PERFORM read_input TABLES lt_lines
                     USING  lv_file.

  CHECK sy-subrc = 0.

* Part 1
  DATA: lt_levels LIKE lt_lines,
        lv_level  TYPE i,
        lv_prev   TYPE i,
        lv_diff   TYPE i,
        lv_flag   TYPE c. "I for increasing, D for decreasing
        
  LOOP AT lt_lines INTO lv_line.
    CLEAR: lt_levels[], lv_prev, lv_flag.
    SPLIT lv_line AT space INTO TABLE lt_levels.
    LOOP AT lt_levels INTO lv_level.
      IF lv_prev IS INITIAL.
        lv_prev = lv_level.
      ELSE.
        lv_diff = abs( lv_level - lv_prev ).
        IF lv_level > lv_prev AND lv_diff <= 3 AND ( lv_flag IS INITIAL OR lv_flag = 'I' ).
          lv_flag = 'I'.
        ELSEIF lv_level < lv_prev AND lv_diff <= 3 AND ( lv_flag IS INITIAL OR lv_Flag = 'D' ).
          lv_flag = 'D'.
        ELSE.
          EXIT.
        ENDIF.
        lv_prev = lv_level.
        CHECK sy-tabix = lines( lt_levels ).
        ADD 1 TO ev_part1.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

* Part 2
  DATA: lv_index TYPE i VALUE 0,
        lv_done  TYPE boolean.

  LOOP AT lt_lines INTO lv_line.
    CLEAR: lt_levels[], lv_prev, lv_flag, lv_index.
    SPLIT lv_line AT space INTO TABLE lt_levels.
    DO.
      LOOP AT lt_levels INTO lv_level.
        IF sy-tabix = lv_index.
          CONTINUE.
        ELSEIF lv_prev IS INITIAL.
          lv_prev = lv_level.
        ELSE.
          lv_diff = abs( lv_level - lv_prev ).
          IF lv_level > lv_prev AND lv_diff <= 3 AND ( lv_flag IS INITIAL OR lv_flag = 'I' ).
            lv_flag = 'I'.
          ELSEIF lv_level < lv_prev AND lv_diff <= 3 AND ( lv_flag IS INITIAL OR lv_Flag = 'D' ).
            lv_flag = 'D'.
          ELSE.
            IF sy-tabix = lines( lt_levels ) AND lv_index IS INITIAL.
              "last index of first iteration, therefore safe
            ELSEIF lv_index <> lines( lt_levels ).
              lv_index = lv_index + 1.
              CLEAR: lv_prev, lv_flag.
              EXIT.
            ELSEIF lv_index = lines( lt_levels ).
              lv_done = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
          lv_prev = lv_level.
          CHECK sy-tabix = lines( lt_levels ).
          ADD 1 TO ev_part2.
          lv_done = 'X'.
        ENDIF.
      ENDLOOP.
      IF lv_done IS NOT INITIAL.
        CLEAR lv_done.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.

ENDFUNCTION.