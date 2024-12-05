FUNCTION znp_aoc_2024_day1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_PART1) TYPE  I
*"     REFERENCE(EV_PART2) TYPE  I
*"----------------------------------------------------------------------
  DATA: lv_file  TYPE string,
        lv_day   TYPE c VALUE 1,
        lt_lines TYPE STANDARD TABLE OF string,
        lv_line  LIKE LINE OF lt_lines.

* Read input
  CONCATENATE gc_filepath lv_day gc_txt INTO lv_file.
  PERFORM read_input TABLES lt_lines
                     USING  lv_file.

  CHECK sy-subrc = 0.

* Part 1
  DATA: lt_left  LIKE lt_lines,
        lt_right LIKE lt_lines,
        lv_left LIKE LINE OF lt_lines,
        lv_right LIKE LINE OF lt_lines,
        lv_diff TYPE i.

  LOOP AT lt_lines INTO lv_line.
    SPLIT lv_line AT space INTO lv_left lv_right.
    CONDENSE: lv_left, lv_right.
    APPEND: lv_left TO lt_left, lv_right TO lt_right.
  ENDLOOP.

  SORT: lt_left ASCENDING, lt_right ASCENDING.

  DO lines( lt_lines ) TIMES.
    READ TABLE: lt_left INTO lv_left INDEX sy-index,
                lt_right INTO lv_right INDEX sy-index.
    lv_diff = abs( lv_left - lv_right ).
    ADD lv_diff TO ev_part1.
  ENDDO.

* Part 2
  TYPES: BEGIN OF id_struc,
           left  TYPE c LENGTH 50,
           right TYPE c LENGTH 50,
         END OF id_struc.
  TYPES: BEGIN OF read_struc,
           id      TYPE i,
           appears TYPE i,
           score   TYPE i,
         END OF read_struc.
  DATA: lt_id   TYPE STANDARD TABLE OF id_struc,
        ls_id   LIKE LINE OF lt_id,
        lt_read TYPE STANDARD TABLE OF read_struc,
        ls_read LIKE LINE OF lt_read,
        lv_appears TYPE i,
        lv_score   TYPE i.

  DO lines( lt_lines ) TIMES.
    READ TABLE: lt_left INTO ls_id-left INDEX sy-index,
                lt_right INTO ls_id-right INDEX sy-index.
    APPEND ls_id TO lt_id.
  ENDDO.

  DO lines( lt_lines ) TIMES.
    READ TABLE lt_id INTO ls_id INDEX sy-index.
    READ TABLE lt_read INTO ls_read WITH KEY id = ls_id-left.
    IF sy-subrc = 0.
      ADD ls_read-score TO ev_part2.
    ELSE.
      CLEAR lv_appears.
      LOOP AT lt_id TRANSPORTING NO FIELDS WHERE right = ls_id-left.
        ADD 1 TO lv_appears.
      ENDLOOP.
      lv_score = ls_id-left * lv_appears.
      ls_read-id = ls_id-left.
      ls_read-appears = lv_appears.
      ls_read-score = lv_score.
      APPEND ls_read TO lt_read.
      ADD lv_score TO ev_part2.
    ENDIF.
  ENDDO.

ENDFUNCTION.