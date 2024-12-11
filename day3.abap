FUNCTION ZNP_AOC_2024_DAY3.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_PART1) TYPE  I
*"     REFERENCE(EV_PART2) TYPE  I
*"----------------------------------------------------------------------
  DATA: lv_file  TYPE string,
        lv_day   TYPE c VALUE 3,
        lt_lines TYPE STANDARD TABLE OF string,
        lv_line  LIKE LINE OF lt_lines.

* Read input
  CONCATENATE gc_filepath lv_day gc_txt INTO lv_file.
  PERFORM read_input TABLES lt_lines
                     USING  lv_file.

  CHECK sy-subrc = 0.

* Part 1
  DATA: lv_mulrgx TYPE string VALUE 'mul\(\d+,\d+\)',
        lv_rmvrgx TYPE string VALUE '[^0-9]',
        lv_substr TYPE string,
        lv_offset TYPE i,
        lv_length TYPE i,
        lv_remove TYPE i,
        lv_val1   TYPE string,
        lv_val2   TYPE string.

  LOOP AT lt_lines INTO lv_line.
    DO.
      FIND REGEX lv_mulrgx IN lv_line MATCH OFFSET lv_offset MATCH LENGTH lv_length.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        lv_substr = lv_line+lv_offset(lv_length).
        SPLIT lv_substr AT ',' INTO lv_val1 lv_val2.
        REPLACE ALL OCCURRENCES OF REGEX lv_rmvrgx IN: lv_val1 WITH '',
                                                       lv_val2 WITH ''.
        lv_remove = lv_offset + lv_length.
        lv_line = lv_line+lv_remove.
        CONDENSE lv_line.
        ev_part1 = ev_part1 + ( lv_val1 * lv_val2 ).
      ENDIF.
    ENDDO.
  ENDLOOP.

* Part 2
  DATA: lv_do      TYPE boolean VALUE 'X',
        lv_dorgx   TYPE string  VALUE 'do\(\)',
        lv_dontrgx TYPE string  VALUE 'don''t\(\)',
        lv_regex   TYPE string.

  CONCATENATE lv_mulrgx '|' lv_dorgx '|' lv_dontrgx INTO lv_regex.

  LOOP AT lt_lines INTO lv_line.
    DO.
      FIND REGEX lv_regex IN lv_line MATCH OFFSET lv_offset MATCH LENGTH lv_length.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        lv_substr = lv_line+lv_offset(lv_length).
        IF lv_substr = 'do()'.
          lv_do = 'X'.
        ELSEIF lv_substr = 'don''t()'.
          CLEAR lv_do.
        ELSEIF lv_do = 'X'. "also means lv_substr = mul...
          SPLIT lv_substr AT ',' INTO lv_val1 lv_val2.
          REPLACE ALL OCCURRENCES OF REGEX lv_rmvrgx IN: lv_val1 WITH '',
                                                         lv_val2 WITH ''.
          ev_part2 = ev_part2 + ( lv_val1 * lv_val2 ).
        ENDIF.
        lv_remove = lv_offset + lv_length.
        lv_line = lv_line+lv_remove.
        CONDENSE lv_line.
      ENDIF.
    ENDDO.
  ENDLOOP.

ENDFUNCTION.