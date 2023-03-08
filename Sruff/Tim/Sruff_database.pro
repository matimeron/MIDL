Pro Sruff_database_event, event

Widget_control, event.top, get_uvalue = d_info
Widget_control, event.id, get_uvalue = eventval

case eventval of

   "D_HELP":  XDisplayFile, "", $
           title = "DATABASE QUERY HELP", $
           group = event.top, $
           width = 60, $
           height = 16, $
           done_button = "Exit", $
           text = ['The computation time required to generate an undulator spectrum', $
                   'file can be close to an hour depending on processor speed and the', $
                   'input parameters.','', $
                   'This window allows the user to select a file from a database', $
                   'of previously generated undulator spectrum to view the input', $
                   'parameters used.', '',$
                   'An undulator spectrum file is required for the following', $
                   'routines:', $
                   '     - Calculate absorbed power in a media', $
                   '     - Calculate transmitted or reflected power through filters or mirrors', $
                   '     - Calculate harmonic size and intensity']


   "D_EXIT": Widget_control, event.top, /destroy

   "PICK": begin
     result = un_info_ts(file = '')
     Widget_control, d_info.d_display, set_value = result.result_string
   endcase

endcase

end


Pro Sruff_database, Group = group

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

if (XRegistered( 'Sruff_database') ne 0) then return
d_base = Widget_base(title = "UNDULATOR SPECTRUM FILE INFORMATION", /row, xoffset = 20, yoffset = 20)
d_base_a = Widget_base(d_base, /column)
d_base_a1 = Widget_base(d_base_a, /row)
d_help = Widget_button(d_base_a1, value = ' HELP ', frame = 0,  /align_left, uvalue = "D_HELP")
d_exit = Widget_button(d_base_a1, value = ' EXIT ', frame = 0, /align_center, $
             uvalue = "D_EXIT")
space = Widget_label(d_base_a, value = '', frame = 0)
space = Widget_label(d_base_a, value = '', frame = 0)
space = Widget_label(d_base_a, value = '', frame = 0)
space = Widget_label(d_base_a, value = '', frame = 0)
space = Widget_label(d_base_a, value = '', frame = 0)
space = Widget_label(d_base_a, value = '', frame = 0)
d_base_a2 = Widget_base(d_base_a, /row)
space = Widget_label(d_base_a2, value = '   ', frame = 0)
Pick_file = Widget_button(d_base_a2, value = '  SELECT A FILE  ', frame = 0, /align_center, $
             uvalue = "PICK")
space = Widget_label(d_base_a2, value = '   ', frame = 0)
d_base_b = Widget_base(d_base, /column)
space = Widget_label(d_base_b, value = '', frame = 0)
d_display = Widget_text(d_base_b, frame = 0, value = '', xsize = 45, ysize = 18)

Widget_control, d_base, /realize
d_info = {d_display:d_display}
Widget_control, d_base, set_uvalue = d_info, /no_copy

XManager, 'Sruff_database', d_base, event_handler = 'Sruff_database_event'

end