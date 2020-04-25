*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

    DEFINE set_if_initial.
      IF &1 IS INITIAL AND &2 IS NOT INITIAL.
        &1 = &2.
      ENDIF.
    END-OF-DEFINITION.

    " Prepare file open/save dialog
    DEFINE prepare_dialog.
      set_if_initial iv_default_extension mv_extension.
      IF iv_default_filename IS INITIAL.
        CONCATENATE '*.' iv_default_extension INTO iv_default_filename.
      ENDIF.

      CASE mv_extension.
        WHEN mc_extension-xlsx.
          set_if_initial iv_file_filter 'Excel Workbook (*.xlsx)'.
        WHEN mc_extension-csv.
          set_if_initial iv_file_filter 'CSV (Tab delimited) (*.csv)'.
        WHEN mc_extension-docx.
          set_if_initial iv_file_filter 'Word Document (*.docx)'.
        WHEN mc_extension-html.
          set_if_initial iv_file_filter cl_gui_frontend_services=>filetype_html.
        WHEN mc_extension-pdf.
          set_if_initial iv_file_filter 'Adobe PDF Files (*.pdf)'.
        WHEN OTHERS.
          set_if_initial iv_file_filter cl_gui_frontend_services=>filetype_all.
      ENDCASE.
    END-OF-DEFINITION.
