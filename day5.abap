FUNCTION znp_aoc_2024_day5.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_PART1) TYPE  I
*"     REFERENCE(EV_PART2) TYPE  I
*"----------------------------------------------------------------------
  DATA: lv_file  TYPE string,
        lv_day   TYPE c VALUE 5,
        lt_lines TYPE STANDARD TABLE OF string,
        lv_line  LIKE LINE OF lt_lines.

* Read input
  CONCATENATE gc_filepath lv_day gc_txt INTO lv_file.
  PERFORM read_input TABLES lt_lines
                     USING  lv_file.

  CHECK sy-subrc = 0.

* Part 1
  DATA: lt_pages   TYPE STANDARD TABLE OF string,
        lv_pages   LIKE LINE OF lt_pages,
        lt_updates TYPE STANDARD TABLE OF string,
        lv_updates LIKE LINE OF lt_updates.

  DATA: lt_order   TYPE STANDARD TABLE OF string,
        lv_rder    LIKE LINE OF lt_order,
        lv_val1    TYPE string,
        lv_val2    TYPE string,
        lv_idx1    TYPE i,
        lv_idx2    TYPE i,
        lv_count   TYPE i,
        lv_middle  TYPE i,
        lv_skip    TYPE i VALUE 0.

  LOOP AT lt_lines INTO lv_line.
    IF lv_line IS INITIAL.
      lv_skip = 1.
      CONTINUE.
    ENDIF.
    IF lv_skip = 1.
      APPEND lv_line TO lt_updates.
    ELSE.
      APPEND lv_line TO lt_pages.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_updates INTO lv_updates.
    lv_skip = 0.
    SPLIT lv_updates AT ',' INTO TABLE lt_order.
    LOOP AT lt_pages INTO lv_pages.
      SPLIT lv_pages AT '|' INTO lv_val1 lv_val2.
      READ TABLE lt_order TRANSPORTING NO FIELDS WITH KEY table_line = lv_val1.
      CHECK sy-subrc = 0.
      lv_idx1 = sy-tabix.
      READ TABLE lt_order TRANSPORTING NO FIELDS WITH KEY table_line = lv_val2.
      CHECK sy-subrc = 0.
      lv_idx2 = sy-tabix.
      IF lv_idx1 > lv_idx2.
        lv_skip = 1.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_skip = 0.
      DESCRIBE TABLE lt_order LINES lv_count.
      READ TABLE lt_order INDEX ( lv_count / 2 ) INTO lv_count.
      ev_part1 = ev_part1 + lv_count.
    ENDIF.
  ENDLOOP.

* Part 2
  DATA: lv_fix TYPE i.

  LOOP AT lt_updates INTO lv_updates.
    lv_fix = 0.
    SPLIT lv_updates AT ',' INTO TABLE lt_order.
    WHILE 1 = 1.
      lv_skip = 0.
      LOOP AT lt_pages INTO lv_pages.
        SPLIT lv_pages AT '|' INTO lv_val1 lv_val2.
        READ TABLE lt_order TRANSPORTING NO FIELDS WITH KEY table_line = lv_val1.
        CHECK sy-subrc = 0.
        lv_idx1 = sy-tabix.
        READ TABLE lt_order TRANSPORTING NO FIELDS WITH KEY table_line = lv_val2.
        CHECK sy-subrc = 0.
        lv_idx2 = sy-tabix.
        IF lv_idx1 > lv_idx2.
          lt_order[ lv_idx1 ] = lv_val2.
          lt_order[ lv_idx2 ] = lv_val1.
          lv_skip = 1.
          lv_fix = 1.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_skip = 0 AND lv_fix = 1. "order was incorrect and is now fixed
        DESCRIBE TABLE lt_order LINES lv_count.
        READ TABLE lt_order INDEX ( lv_count / 2 ) INTO lv_count.
        ev_part2 = ev_part2 + lv_count.
        EXIT.
      ELSEIF lv_skip = 0 AND lv_fix = 0. "order was already correct
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDLOOP.
ENDFUNCTION.