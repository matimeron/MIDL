Pro Show_event, event

common show_plot_info, glob, x, y, out_levs, lin_out, annot_out, xtit, $
                     ytit, ztit, title_out, subtitle_out
common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

Widget_control, event.top, get_uvalue = s, /no_copy
Widget_control, event.id, get_uvalue = eventvalue

if N_Elements(s.s_cur->Get_Value()) eq 0 then s.s_cur->Set_Value, 0.
if N_Elements(s.s_dist->Get_Value()) eq 0 then s.s_dist->Set_Value, 0.
if N_Elements(s.s_sigx->Get_Value()) eq 0 then s.s_sigx->Set_Value, 0.
if N_Elements(s.s_sigy->Get_Value()) eq 0 then s.s_sigy->Set_Value, 0.
if N_Elements(s.s_sigx_prim->Get_Value()) eq 0 then s.s_sigx_prim->Set_Value, 0.
if N_Elements(s.s_sigy_prim->Get_Value()) eq 0 then s.s_sigy_prim->Set_Value, 0.
if N_Elements(s.filter_1->Get_Value()) eq 0 then s.filter_1->Set_Value, ''
if N_Elements(s.filter_2->Get_Value()) eq 0 then s.filter_2->Set_Value, ''
if N_Elements(s.filter_3->Get_Value()) eq 0 then s.filter_3->Set_Value, ''
if N_Elements(s.filter_4->Get_Value()) eq 0 then s.filter_4->Set_Value, ''
if N_Elements(s.filter_5->Get_Value()) eq 0 then s.filter_5->Set_Value, ''
if N_Elements(s.filter_6->Get_Value()) eq 0 then s.filter_6->Set_Value, ''
if N_Elements(s.mirror_1->Get_Value()) eq 0 then s.mirror_1->Set_Value, ''
if N_Elements(s.mirror_2->Get_Value()) eq 0 then s.mirror_2->Set_Value, ''
if N_Elements(s.mirror_3->Get_Value()) eq 0 then s.mirror_3->Set_Value, ''
if N_Elements(s.mirror_4->Get_Value()) eq 0 then s.mirror_4->Set_Value, ''
if N_Elements(s.mirror_5->Get_Value()) eq 0 then s.mirror_5->Set_Value, ''
if N_Elements(s.mirror_6->Get_Value()) eq 0 then s.mirror_6->Set_Value, ''
if N_Elements(s.thick_1->Get_Value()) eq 0 then s.thick_1->Set_Value, 0.
if N_Elements(s.thick_2->Get_Value()) eq 0 then s.thick_2->Set_Value, 0.
if N_Elements(s.thick_3->Get_Value()) eq 0 then s.thick_3->Set_Value, 0.
if N_Elements(s.thick_4->Get_Value()) eq 0 then s.thick_4->Set_Value, 0.
if N_Elements(s.thick_5->Get_Value()) eq 0 then s.thick_5->Set_Value, 0.
if N_Elements(s.thick_6->Get_Value()) eq 0 then s.thick_6->Set_Value, 0.
if N_Elements(s.angle_1->Get_Value()) eq 0 then s.angle_1->Set_Value, 0.
if N_Elements(s.angle_2->Get_Value()) eq 0 then s.angle_2->Set_Value, 0.
if N_Elements(s.angle_3->Get_Value()) eq 0 then s.angle_3->Set_Value, 0.
if N_Elements(s.angle_4->Get_Value()) eq 0 then s.angle_4->Set_Value, 0.
if N_Elements(s.angle_5->Get_Value()) eq 0 then s.angle_5->Set_Value, 0.
if N_Elements(s.angle_6->Get_Value()) eq 0 then s.angle_6->Set_Value, 0.

miliAmp = s.s_cur->get_value()
dist_s = s.s_dist->get_value()
sigx_s = s.s_sigx->get_value()
sigy_s = s.s_sigy->get_value()
sigx_prim_s = s.s_sigx_prim->get_value()
sigy_prim_s = s.s_sigy_prim->get_value()
winx_s = s.s_winx->get_value()
winy_s = s.s_winy->get_value()
f_1 = s.filter_1->get_value()
f_2 = s.filter_2->get_value()
f_3 = s.filter_3->get_value()
f_4 = s.filter_4->get_value()
f_5 = s.filter_5->get_value()
f_6 = s.filter_6->get_value()
t_1 = s.thick_1->get_value()
t_2 = s.thick_2->get_value()
t_3 = s.thick_3->get_value()
t_4 = s.thick_4->get_value()
t_5 = s.thick_5->get_value()
t_6 = s.thick_6->get_value()
m_1 = s.mirror_1->get_value()
m_2 = s.mirror_2->get_value()
m_3 = s.mirror_3->get_value()
m_4 = s.mirror_4->get_value()
m_5 = s.mirror_5->get_value()
m_6 = s.mirror_6->get_value()
a_1 = s.angle_1->get_value()
a_2 = s.angle_2->get_value()
a_3 = s.angle_3->get_value()
a_4 = s.angle_4->get_value()
a_5 = s.angle_5->get_value()
a_6 = s.angle_6->get_value()
Widget_control, s.s_display, get_value = display_choice


if s.filterIndex eq 1 then begin
   filts = [f_1]
   filths = [t_1]
endif
if s.filterIndex eq 2 then begin
   filts = [f_1, f_2]
   filths = [t_1, t_2]
endif
if s.filterIndex eq 3 then begin
   filts = [f_1, f_2, f_3]
   filths = [t_1, t_2, t_3]
endif
if s.filterIndex eq 4 then begin
   filts = [f_1, f_2, f_3, f_4]
   filths = [t_1, t_2, t_3, t_4]
endif
if s.filterIndex eq 5 then begin
   filts = [f_1, f_2, f_3, f_4, f_5]
   filths = [t_1, t_2, t_3, t_4, t_5]
endif
if s.filterIndex eq 6 then begin
   filts = [f_1, f_2, f_3, f_4, f_5, f_6]
   filths = [t_1, t_2, t_3, t_4, t_5, t_6]
endif

if s.mirrorIndex eq 1 then begin
   mirrs = [m_1]
   mirans = [a_1]
endif
if s.mirrorIndex eq 2 then begin
   mirrs = [m_1, m_2]
   mirans = [a_1, a_2]
endif
if s.mirrorIndex eq 3 then begin
   mirrs = [m_1, m_2, m_3]
   mirans = [a_1, a_2, a_3]
endif
if s.mirrorIndex eq 4 then begin
   mirrs = [m_1, m_2, m_3, m_4]
   mirans = [a_1, a_2, a_3, a_4]
endif
if s.mirrorIndex eq 5 then begin
   mirrs = [m_1, m_2, m_3, m_4, m_5]
   mirans = [a_1, a_2, a_3, a_4, a_5]
endif
if s.mirrorIndex eq 6 then begin
   mirrs = [m_1, m_2, m_3, m_4, m_5, m_6]
   mirans = [a_1, a_2, a_3, a_4, a_5, a_6]
endif


cur_s = miliAmp/1000.
rsig_s = [sigx_s, sigy_s]
asig_s = [sigx_prim_s, sigy_prim_s]
win_s = [winx_s, winy_s]

Wset, s.plot_index


case eventvalue of

   "FILTER_NUM": begin
      f_bases = [s.filter1_base, s.filter2_base, s.filter3_base, $
                 s.filter4_base, s.filter5_base, s.filter6_base]

      s.filterIndex = event.index
      if event.index gt 0 then begin
         for i = 0, (event.index - 1) do begin
            Widget_control, f_bases(i), map = 1
         endfor
      endif
      if event.index lt 6 then begin
         for i = (event.index), 5 do begin
            Widget_control, f_bases(i), map = 0
         endfor
      endif
      new_info = s
   endcase

   "MIRROR_NUM": begin
      m_bases = [s.mirror1_base, s.mirror2_base, s.mirror3_base, $
                 s.mirror4_base, s.mirror5_base, s.mirror6_base]

      s.mirrorIndex = event.index
      if event.index gt 0 then begin
         for i = 0, (event.index - 1) do begin
            Widget_control, m_bases(i), map = 1
         endfor
      endif
      if event.index lt 6 then begin
         for i = (event.index), 5 do begin
            Widget_control, m_bases(i), map = 0
         endfor
      endif
      new_info = s
   endcase

   "S_CALCULATE": begin
      new_info = s
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      ;**********Begin input error checking**********
      if miliamp le 0. then begin
         message_out = DIALOG_MESSAGE( ['Storage ring beam current cannot be negative or zero!','',$
                                  'Please enter a value for the beam current (unit = mA).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif
     if dist_s le 0. then begin
         message_out = DIALOG_MESSAGE( ['Distance to source cannot be negative or zero!','',$
                                  'Please enter a value for the distance to source (unit = m).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif
     if s.filename eq '' then begin
        message_out = DIALOG_MESSAGE( ['A spectrum file must be selected','',$
                                  'Please select a spectrum file.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif

     if winx_s le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture x size cannot be negative or zero!','',$
                                  'Please enter a value for the x size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif
     if winy_s le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture y size cannot be negative or zero!','',$
                                  'Please enter a value for the y size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif
     if s.filterIndex ne 0 then begin
        for i = 0, s.filterIndex - 1 do begin
           if filts(i) eq '' then begin
              message_out = DIALOG_MESSAGE( ['Filter material field must contain information','',$
                                  'Please enter the symbol of an element for the filter material.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = new_info, /no_copy
              endif
              return
           endif
           if filths(i) le 0 then begin
              message_out = DIALOG_MESSAGE( ['Filter thickness cannot be negative or zero','',$
                                  'Please enter a value for the filter thickness. (unit = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = new_info, /no_copy
              endif
              return
           endif
        endfor
     endif
     if s.mirrorIndex ne 0 then begin
        for i = 0, s.mirrorIndex - 1 do begin
           if mirrs(i) eq '' then begin
              message_out = DIALOG_MESSAGE( ['Mirror coating field must contain information','',$
                                  'Please enter the symbol of an element for the mirror coating.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = new_info, /no_copy
              endif
              return
           endif
           if mirans(i) le 0 then begin
              message_out = DIALOG_MESSAGE( ['Mirror angles cannot be negative or zero','',$
                                  'Please enter a value for the mirror angle. (unit = mrad)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = new_info, /no_copy
              endif
              return
           endif
        endfor
     endif
     ;***********End input error checking***********
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      Widget_control, s.s_display, set_value = 0
      name_s = s.filename
      if s.filterIndex gt 0 then begin
         if s.mirrorIndex gt 0 then begin
            un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s, $
                 /smear, window = win_s, filters = filts, filthicks = filths, $
                  mirrors = mirrs, mirangles = mirans, outpower = opow
         endif else un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s, $
                 /smear, window = win_s, filters = filts, filthicks = filths, $
                  outpower = opow
      endif

      if s.filterIndex eq 0 then begin
         if s.mirrorIndex gt 0 then begin
            un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s, $
                 /smear, window = win_s, mirrors = mirrs, mirangles = mirans, $
                 outpower = opow
         endif else un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s, $
                 /smear, window = win_s, outpower = opow
      endif

      Widget_control, event.top, /sensitive
      Widget_control, s.sbase1b, sensitive = 0
      Widget_control, s.sbase2a, /sensitive
      Widget_control, s.s_newbase, /sensitive

      new_info = {sbase1b:s.sbase1b, s_cur:s.s_cur, s_dist:s.s_dist, s_text:s.s_text, $
       s_sigx:s.s_sigx, s_sigy:s.s_sigy, s_sigx_prim:s.s_sigx_prim, s_sigy_prim:s.s_sigy_prim, $
       s_winx:s.s_winx, s_winy:s.s_winy, filter_number:s.filter_number, $
       mirror_number:s.mirror_number, filter1_base:s.filter1_base, filter2_base:s.filter2_base, $
       filter3_base:s.filter3_base, filter4_base:s.filter4_base, filter5_base:s.filter5_base, $
       filter6_base:s.filter6_base, mirror1_base:s.mirror1_base, mirror2_base:s.mirror2_base, $
       mirror3_base:s.mirror3_base, mirror4_base:s.mirror4_base, mirror5_base:s.mirror5_base, $
       mirror6_base:s.mirror6_base, filter_1:s.filter_1, filter_2:s.filter_2, filter_3:s.filter_3, $
       filter_4:s.filter_4, filter_5:s.filter_5, filter_6:s.filter_6, thick_1:s.thick_1, $
       thick_2:s.thick_2, thick_3:s.thick_3, thick_4:s.thick_4, thick_5:s.thick_5, thick_6:s.thick_6, $
       mirror_1:s.mirror_1, mirror_2:s.mirror_2, mirror_3:s.mirror_3, mirror_4:s.mirror_4, $
       mirror_5:s.mirror_5, mirror_6:s.mirror_6, angle_1:s.angle_1, angle_2:s.angle_2, $
       angle_3:s.angle_3, angle_4:s.angle_4, angle_5:s.angle_5, angle_6:s.angle_6, $
       filename:s.filename, plot_index:s.plot_index, sbase2a:s.sbase2a, s_newbase:s.s_newbase, $
       filterIndex:s.filterIndex, mirrorIndex:s.mirrorIndex, s_display:s.s_display, s_outpower:opow}
   endcase

   "S_DISPLAY": begin
      if display_choice eq 0 then begin
         contour, glob, x, y, $
		 levels = out_levs, c_line = lin_out, c_annotation = annot_out, $
		 xtit = xtit, ytit = ytit, $
		 title = title_out, subtitle = subtitle_out, ymargin = [6,4]
      endif
      ;surface plot
      if display_choice eq 1 then begin
         surface, glob, x, y, xtit = xtit, ytit = ytit, ztit = ztit, $
	  	 title = title_out, subtitle = subtitle_out
      endif
      ; shaded surface plot
      if display_choice eq 2 then begin
         shade_surf, glob, x, y, xtit = xtit, ytit = ytit, ztit = ztit, $
	  	 title = title_out, subtitle = subtitle_out
      endif
      new_info = s
   endcase

   "S_NEWCALC": begin
      Widget_control, s.sbase1b, /sensitive
      Widget_control, s.sbase2a, sensitive = 0
      Widget_control, s.s_newbase, sensitive = 0
      new_info = s
   endcase

   "SELECT": begin
      result = un_info_ts(file = '')
     Widget_control, s.s_text, set_value = result.result_string
     s.filename = result.result_string(0)
     temp_winx = result.alims(0) * dist_s
     temp_winy = result.alims(1) * dist_s
     s.s_winx->set_value, temp_winx
     s.s_winy->set_value, temp_winy
     new_info = s
   endcase

   "SHOW_EXIT": begin
      Widget_control, event.top, /destroy
      return
   endcase

   "S_FIT": begin
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      y_n_slice = 0
      depths = ''
      Sruff_fit, s.s_outpower, winx_s, winy_s, y_n_slice, depths,  Group = event.top
      new_info = s
      endcase

   "SHOW_HELP": begin
       XDisplayFile, "", $
       title = "POWER TRANSMISSION HELP", $
       group = event.top, $
       width = 70, $
       height = 45, $
       done_button = "Exit", $
       text = ['This routine calculates the transmitted or reflected power from filters and mirrors.', '','', $
               'NOTE: Default parameters loaded are for APS storage ring lattice.','','', $
               'Note: A spectrum file must be selected before calculation can be made.', $
               'Spectrum may be chosen from the existing database or generated using ', $
               'the GENERATE UNDULATOR SPECTRUM routine.','',$
               'Storage ring beam current: input unit = mA,  example: 100','',$
               'Distance to source:  input unit = meter,  example: 30','',$

               'Beam size:  input unit = millimeter',$
               '                   example:  sigx: 0.352',$
               '                                   sigy: 0.018','',$
               'Beam divergence:  input unit = mrad',$
               '                              example:  sigx`: 0.022',$
               '                                              sigy`: 0.0042','',$
               'Full aperture:  full aperture size,  input unit = millimeter',$
               '                      example:  xsize: 10',$
               '                                      ysize: 8','', $
               'Select the appropriate number of filters and mirrors', '', $
               'filter material:  enter an element symbol according to the atomic table', $
               '                      example: c for carbon','',$
               'filter thickness:  input unit = millimeter', $
               '                         example: 0.3', '', $
               'mirror coating:  enter an element symbol according to the atomic table', $
               '                        example: si for silicon','',$
               'mirror angle:  input unit = milliradian', $
               '                     example: 4.0']
      new_info = s
   endcase

   "S_PRINT": begin
      printer_choice = dialog_printersetup()
      if printer_choice eq 0 then begin
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = s, /no_copy
         endif
         return
      endif
      name_s = s.filename
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if s.filterIndex gt 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, filters = filts, filthicks = filths,'  + $
                 'mirrors = mirrs, mirangles = mirans', $
                  sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s, filts, filths, mirrs, mirans', cur_s, dist_s, rsig_s, $
                  asig_s, name_s, win_s, filts, filths, mirrs, mirans, $
                  DEVICE = 'PRINTER'
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
               '/smear, window = win_s, filters = filts, filthicks = filths', $
               sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths', $
               cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths, $
               DEVICE = 'PRINTER'
         endif
         if s.filterIndex eq 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, mirrors = mirrs, mirangles = mirans', $
                 sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans', $
                 cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans, $
                 DEVICE = 'PRINTER'
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s', sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s', cur_s, dist_s, rsig_s, asig_s, name_s, win_s, DEVICE = 'PRINTER'
         endif
      endif
      if display_choice eq 1 then begin
         if s.filterIndex gt 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, filters = filts, filthicks = filths, '  + $
                 'mirrors = mirrs, mirangles = mirans, /surface', $
                  sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s, filts, filths, mirrs, mirans', cur_s, dist_s, rsig_s, $
                  asig_s, name_s, win_s, filts, filths, mirrs, mirans, $
                  DEVICE = 'PRINTER'
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
               '/smear, window = win_s, filters = filts, filthicks = filths, /surf', $
               sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths', $
               cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths, $
               DEVICE = 'PRINTER'
         endif
         if s.filterIndex eq 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, mirrors = mirrs, mirangles = mirans, /surf', $
                 sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans', $
                 cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans, $
                 DEVICE = 'PRINTER'
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, /surf', sub = 'cur_s, dist_s, rsig_s, asig_s,' + $
                 'name_s, win_s', cur_s, dist_s, rsig_s, asig_s, name_s, win_s, DEVICE = 'PRINTER'
         endif
      endif
      if display_choice eq 2 then begin
         if s.filterIndex gt 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, filters = filts, filthicks = filths, '  + $
                 'mirrors = mirrs, mirangles = mirans, /shade', $
                  sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s, filts, filths, mirrs, mirans', cur_s, dist_s, rsig_s, $
                  asig_s, name_s, win_s, filts, filths, mirrs, mirans, $
                  DEVICE = 'PRINTER'
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
               '/smear, window = win_s, filters = filts, filthicks = filths, /shade', $
               sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths', $
               cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths, $
               DEVICE = 'PRINTER'
         endif
         if s.filterIndex eq 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, mirrors = mirrs, mirangles = mirans, /shade', $
                 sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans', $
                 cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans, $
                 DEVICE = 'PRINTER'
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, /shade', sub = 'cur_s, dist_s, rsig_s, asig_s,' + $
                 'name_s, win_s ', cur_s, dist_s, rsig_s, asig_s, name_s, win_s, $
                 DEVICE = 'PRINTER'
         endif
      endif
      Widget_control, event.top, /sensitive
      new_info = s
   endcase

"S_SAVE": begin
     s_outfile = dialog_pickfile(dialog_parent = event.top, get_path=path_s, $
     path = 'C:\my documents',title = 'Please Select a Folder and Enter a Filename', /write)
     if s_outfile eq '' then begin
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = s, /no_copy
        endif
        return
     endif
      name_s = s.filename
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if s.filterIndex gt 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, filters = filts, filthicks = filths,'  + $
                 'mirrors = mirrs, mirangles = mirans', $
                  sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s, filts, filths, mirrs, mirans', cur_s, dist_s, rsig_s, $
                  asig_s, name_s, win_s, filts, filths, mirrs, mirans, $
                  DEVICE = 'EPS', FIL = s_outfile
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
               '/smear, window = win_s, filters = filts, filthicks = filths', $
               sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths', $
               cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths, $
               DEVICE = 'EPS', FIL = s_outfile
         endif
         if s.filterIndex eq 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, mirrors = mirrs, mirangles = mirans', $
                 sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans', $
                 cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans, $
                 DEVICE = 'EPS', FIL = s_outfile
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s', sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s', cur_s, dist_s, rsig_s, asig_s, name_s, win_s, DEVICE = 'EPS', FIL = s_outfile
         endif
      endif
      if display_choice eq 1 then begin
         if s.filterIndex gt 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, filters = filts, filthicks = filths, '  + $
                 'mirrors = mirrs, mirangles = mirans, /surface', $
                  sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s, filts, filths, mirrs, mirans', cur_s, dist_s, rsig_s, $
                  asig_s, name_s, win_s, filts, filths, mirrs, mirans, $
                  DEVICE = 'EPS', FIL = s_outfile
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
               '/smear, window = win_s, filters = filts, filthicks = filths, /surf', $
               sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths', $
               cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths, $
               DEVICE = 'EPS', FIL = s_outfile
         endif
         if s.filterIndex eq 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, mirrors = mirrs, mirangles = mirans, /surf', $
                 sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans', $
                 cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans, $
                 DEVICE = 'EPS', FIL = s_outfile
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, /surf', sub = 'cur_s, dist_s, rsig_s, asig_s,' + $
                 'name_s, win_s', cur_s, dist_s, rsig_s, asig_s, name_s, win_s, DEVICE = 'EPS', $
                 FIL = s_outfile
         endif
      endif
      if display_choice eq 2 then begin
         if s.filterIndex gt 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, filters = filts, filthicks = filths, '  + $
                 'mirrors = mirrs, mirangles = mirans, /shade', $
                  sub = 'cur_s, dist_s, rsig_s, asig_s, name_s,' + $
                 'win_s, filts, filths, mirrs, mirans', cur_s, dist_s, rsig_s, $
                  asig_s, name_s, win_s, filts, filths, mirrs, mirans, $
                  DEVICE = 'EPS', FIL = s_outfile
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
               '/smear, window = win_s, filters = filts, filthicks = filths, /shade', $
               sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths', $
               cur_s, dist_s, rsig_s, asig_s, name_s, win_s, filts, filths, $
               DEVICE = 'EPS', FIL = s_outfile
         endif
         if s.filterIndex eq 0 then begin
            if s.mirrorIndex gt 0 then begin
               output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, mirrors = mirrs, mirangles = mirans, /shade', $
                 sub = 'cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans', $
                 cur_s, dist_s, rsig_s, asig_s, name_s, win_s, mirrs, mirans, $
                 DEVICE = 'EPS', FIL = s_outfile
            endif else output, 'un_show_ts, cur_s, dist_s, rsig_s, asig_s, file = name_s,' + $
                 '/smear, window = win_s, /shade', sub = 'cur_s, dist_s, rsig_s, asig_s,' + $
                 'name_s, win_s', cur_s, dist_s, rsig_s, asig_s, name_s, win_s, $
                 DEVICE = 'EPS', FIL = s_outfile
         endif
      endif
      Widget_control, event.top, /sensitive
      new_info = s
   endcase

endcase

f =[s.filter_1, s.thick_1, s.filter_2, s.thick_2, s.filter_3, s.thick_3, $
    s.filter_4, s.thick_4, s.filter_5, s.thick_5, s.filter_6, s.thick_6]

m =[s.mirror_1, s.angle_1, s.mirror_2, s.angle_2, s.mirror_3, s.angle_3, $
    s.mirror_4, s.angle_4, s.mirror_5, s.angle_5, s.mirror_6, s.angle_6]

fIndex = 2*s.filterIndex
mIndex = 2*s.mirrorIndex

if (fIndex eq 0) and (mIndex eq 0) then begin
   s.s_winy->SetTabNext, s.s_cur->GetTextID()
endif

if (fIndex eq 0) and (mIndex gt 0) then begin
   s.s_winy->SetTabNext, m(0)->GetTextID()
   for j = 0, (mIndex - 2) do begin
      m(j)->SetTabNext, m(j+1)->GetTextID()
   endfor
   m(mIndex - 1)->SetTabNext, s.s_cur->GetTextID()
endif

if (fIndex gt 0) and  (mIndex eq 0) then begin
   s.s_winy->SetTabNext, f(0)->GetTextID()
   for i = 0, (fIndex -2 ) do begin
      f(i)->SetTabNext, f(i+1)->GetTextID()
   endfor
   f(fIndex - 1)->SetTabNext, s.s_cur->GetTextID()
endif

if (fIndex gt 0) and (mIndex gt 0) then begin
   s.s_winy->SetTabNext, f(0)->GetTextID()
   for i = 0, (fIndex - 2) do begin
      f(i)->SetTabNext, f(i+1)->GetTextID()
   endfor
   f(fIndex - 1)->SetTabNext, m(0)->GetTextID()
   for j = 0, (mIndex - 2) do begin
      m(j)->SetTabNext, m(j+1)->GetTextID()
   endfor
   m(mIndex - 1)->SetTabNext, s.s_cur->GetTextID()
endif

if Widget_info(event.top, /valid_id) then begin
        Widget_control, event.top, set_uvalue = new_info, /no_copy
endif
end



Pro Show, Group = group

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

if (XRegistered( 'Show') ne 0) then return

show_base = Widget_base(title = "POWER TRANSMISSION CALCULATION", xoffset = 20, yoffset = 20, /row)
sbase1 = Widget_base(show_base, /column)
sbase2 = Widget_base(show_base, /column)
sbase1a = Widget_base(sbase1, /row)
s_help= Widget_button(sbase1a, /align_left, value =' Help ', uvalue ="SHOW_HELP")
s_exit = Widget_button(sbase1a, /align_left, value =' Exit ', uvalue ="SHOW_EXIT")
sbase1b = Widget_base(sbase1, /column)
label = Widget_label(sbase1b, /align_left,  frame = 0, $
        value ='NOTE: DEFAULT PARAMETERS LOADED ARE FOR APS UNDULATOR A' )
space = Widget_label(sbase1b, frame = 0, value ='')
sbase1b1 = Widget_base(sbase1b, /row)
s_cur = FSC_Inputfield(sbase1b1, title ='Storage ring beam current (mA):', $
                      /column, /floatvalue, value = 100)
space = Widget_label(sbase1b1, frame = 0, value ='      ')
s_dist = FSC_Inputfield(sbase1b1, title ='Distance to source (m):', $
                      /column, /floatvalue, value = 30)
sbase1b2 = Widget_base(sbase1b, /row)
sbase1b2a = Widget_base(sbase1b2, /column)
s_file = Widget_button(sbase1b2a,  frame = 0, /align_center, value ='SELECT SPECTRUM FILE', uvalue ="SELECT")
space = Widget_label(sbase1b2a, frame = 0, value ='')
s_text = Widget_text(sbase1b2a, frame = 0, value = '', xsize = 45, ysize = 16)
sbase1b2b = Widget_base(sbase1b2, /column)
label = Widget_label(sbase1b2b, /align_left,  frame = 0, value ='Beam size (mm):')
s_sigx = FSC_Inputfield(sbase1b2b, title ='sigx:', /floatvalue, decimal = 5, value = 0.352)
s_sigy = FSC_Inputfield(sbase1b2b, title ='sigy:', /floatvalue, decimal = 5, value = 0.018)
space = Widget_label(sbase1b2b, frame = 0, value ='')
label = Widget_label(sbase1b2b, /align_left,  frame=0, value ='Beam divergence (mrad):')
s_sigx_prim = FSC_Inputfield(sbase1b2b, title ='sigx`:', /floatvalue, decimal = 5, value = 0.022)
s_sigy_prim = FSC_Inputfield(sbase1b2b, title ='sigy`:', /floatvalue, decimal = 5, value = 0.0042)
space = Widget_label(sbase1b2b, frame = 0, value ='')
label = Widget_label(sbase1b2b, frame = 0, /align_left, value='Aperture size (quadrant, mm):')
s_winx = FSC_Inputfield(sbase1b2b, Title='xsize:', /floatvalue, value = 0, decimal = 2)
s_winy = FSC_Inputfield(sbase1b2b, Title='ysize:', /floatvalue, value = 0, decimal = 2)
space = Widget_label(sbase1b, frame = 0, value ='')
optics_base = Widget_base(sbase1b, /align_left, /column)
label = Widget_label(optics_base, frame = 0, /align_left, $
           value ='Optics parameters for beam transmitted through a series of filters and mirrors:')
number_base = Widget_base(optics_base, frame = 0, /row)
filter_number = Widget_droplist(number_base, title = 'Select number of filters:', $
                                uvalue = "FILTER_NUM", value = ['0','1','2','3','4', '5', '6'])
Widget_control, filter_number, set_droplist_select = 1
space = Widget_label(number_base, frame = 0, value ='     ')
mirror_number = Widget_droplist(number_base, title = 'Select number of mirrors:', $
               uvalue = "MIRROR_NUM", value = ['0','1','2','3','4', '5', '6'])
filter1_base = Widget_base(optics_base, /align_left, /row)
filter_1 = FSC_Inputfield(filter1_base, Title='filter #1 material:', value = 'C')
thick_1 = FSC_Inputfield(filter1_base, Title='filter #1 thickness (mm):', value = 0.3, /floatvalue, $
                         decimal = 2)
filter2_base = Widget_base(optics_base, /align_left, /row, map = 0 )
filter_2 = FSC_Inputfield(filter2_base, Title='filter #2 material:', value = '')
thick_2 = FSC_Inputfield(filter2_base, Title='filter #2 thickness (mm):', value = 0., /floatvalue, $
                         decimal = 2)
filter3_base = Widget_base(optics_base, /align_left, /row, map = 0 )
filter_3 = FSC_Inputfield(filter3_base, Title='filter #3 material:', value = '')
thick_3 = FSC_Inputfield(filter3_base, Title='filter #3 thickness (mm):', value = 0., /floatvalue, $
                         decimal = 2)
filter4_base = Widget_base(optics_base, /align_left, /row, map = 0 )
filter_4 = FSC_Inputfield(filter4_base, Title='filter #4 material:', value = '')
thick_4 = FSC_Inputfield(filter4_base, Title='filter #4 thickness (mm):', value = 0., /floatvalue, $
                         decimal = 2)
filter5_base = Widget_base(optics_base, /align_left, /row, map = 0 )
filter_5 = FSC_Inputfield(filter5_base, Title='filter #5 material:', value = '')
thick_5 = FSC_Inputfield(filter5_base, Title='filter #5 thickness (mm):', value = 0., /floatvalue, $
                         decimal = 2)
filter6_base = Widget_base(optics_base, /align_left, /row, map = 0 )
filter_6 = FSC_Inputfield(filter6_base, Title='filter #6 material:', value = '')
thick_6 = FSC_Inputfield(filter6_base, Title='filter #6 thickness (mm):', value = 0., /floatvalue, $
                         decimal = 2)
space = Widget_label(optics_base, frame = 0, value ='')
mirror1_base = Widget_base(optics_base, /align_left, /row, map = 0 )
mirror_1 = FSC_Inputfield(mirror1_base, Title='mirror #1 coating:', value = '')
angle_1 = FSC_Inputfield(mirror1_base, Title='mirror #1 angle (mrad):', value = 0., /floatvalue, $
                         decimal = 2)
mirror2_base = Widget_base(optics_base, /align_left, /row, map = 0 )
mirror_2 = FSC_Inputfield(mirror2_base, Title='mirror #2 coating:', value = '')
angle_2 = FSC_Inputfield(mirror2_base, Title='mirror #2 angle (mrad):', value = 0., /floatvalue, $
                         decimal = 2)
mirror3_base = Widget_base(optics_base, /align_left, /row, map = 0 )
mirror_3 = FSC_Inputfield(mirror3_base, Title='mirror #3 coating:', value = '')
angle_3 = FSC_Inputfield(mirror3_base, Title='mirror #3 angle (mrad):', value = 0., /floatvalue, $
                         decimal = 2)
mirror4_base = Widget_base(optics_base, /align_left, /row, map = 0 )
mirror_4 = FSC_Inputfield(mirror4_base, Title='mirror #4 coating:', value = '')
angle_4 = FSC_Inputfield(mirror4_base, Title='mirror #4 angle (mrad):', value = 0., /floatvalue, $
                         decimal = 2)
mirror5_base = Widget_base(optics_base, /align_left, /row, map = 0 )
mirror_5 = FSC_Inputfield(mirror5_base, Title='mirror #5 coating:', value = '')
angle_5 = FSC_Inputfield(mirror5_base, Title='mirror #5 angle (mrad):', value = 0., /floatvalue, $
                         decimal = 2)
mirror6_base = Widget_base(optics_base, /align_left, /row, map = 0 )
mirror_6 = FSC_Inputfield(mirror6_base, Title='mirror #6 coating:', value = '')
angle_6 = FSC_Inputfield(mirror6_base, Title='mirror #6 angle (mrad):', value = 0., /floatvalue, $
                         decimal = 2)
s_calculate = Widget_button(sbase1b, /align_center, value ='           CALCULATE          ', $
                            uvalue ="S_CALCULATE")
space = Widget_label(sbase2, frame = 0, value ='')
s_plot = Widget_draw(sbase2, /frame, xsize = 730, ysize = 730)
space = Widget_label(sbase2, frame = 0, value ='')
space = Widget_label(sbase2, frame = 0, value ='')
space = Widget_label(sbase2, frame = 0, value ='')
sbase2a = Widget_base(sbase2, frame = 0, sensitive = 0, /row, /align_center)
plot_base = Widget_base(sbase2a, frame = 1, /column, /align_center)
label = Widget_label(plot_base, /align_center,  frame = 0, value ='Display:')
s_display = CW_bgroup(plot_base, /row, /exclusive, /no_release, $
                   ['contour', 'surface', 'shaded surface'], uvalue ="S_DISPLAY", set_value = 0)
space = Widget_label(sbase2a, frame = 0, value ='     ')
output_base = Widget_base(sbase2a, frame = 1, /align_center, /column)
label = Widget_label(output_base, /align_center ,  frame = 0, value ='Output options:')
space = Widget_Label(output_base, frame = 0, value ='')
output1 = Widget_base(output_base,/align_center, frame = 0, /row)
s_print = Widget_button(output1, frame = 0, /align_center, value ='         PRINT          ', $
                        uvalue ="S_PRINT")
s_save = Widget_button(output1, frame = 0, /align_center, value ='  SAVE AS EPS FILE  ', uvalue ="S_SAVE")
space = Widget_label(sbase2, frame = 0, value ='')
s_newbase = Widget_base(sbase2, frame = 0, sensitive = 0, /align_center, /row)
s_fit = Widget_button(s_newbase, frame = 0, value ='    FIT TO A FORMULA    ', uvalue ="S_FIT")
s_newcalc = Widget_button(s_newbase, frame = 0, value =' NEW POWER TRANSMISSION CALCULATION ', $
                        uvalue = "S_NEWCALC")

s_cur->SetTabNext, s_dist->GetTextID()
s_dist->SetTabNext, s_sigx->GetTextID()
s_sigx->SetTabNext, s_sigy->GetTextID()
s_sigy->SetTabNext, s_sigx_prim->GetTextID()
s_sigx_prim->SetTabNext, s_sigy_prim->GetTextID()
s_sigy_prim->SetTabNext, s_winx->GetTextID()
s_winx->SetTabNext, s_winy->GetTextID()
s_winy->SetTabNext, filter_1->GetTextID()
filter_1->SetTabNext, thick_1->GetTextID()
thick_1->SetTabNext, s_cur->GetTextID()

Widget_control, show_base, /realize
Widget_control, s_plot, get_value = plot_index
filterIndex = 1
mirrorIndex = 0
filename = ''

s = {sbase1b:sbase1b, s_cur:s_cur, s_dist:s_dist, s_text:s_text, s_sigx:s_sigx, s_sigy:s_sigy, $
          s_sigx_prim:s_sigx_prim, s_sigy_prim:s_sigy_prim, s_winx:s_winx, s_winy:s_winy, $
          filter_number:filter_number, mirror_number:mirror_number, $
          filter1_base:filter1_base, filter2_base:filter2_base, filter3_base:filter3_base, $
          filter4_base:filter4_base, filter5_base:filter5_base, filter6_base:filter6_base, $
          mirror1_base:mirror1_base, mirror2_base:mirror2_base, mirror3_base:mirror3_base, $
          mirror4_base:mirror4_base, mirror5_base:mirror5_base, mirror6_base:mirror6_base, $
          filter_1:filter_1, filter_2:filter_2, filter_3:filter_3, filter_4:filter_4, $
          filter_5:filter_5, filter_6:filter_6, thick_1:thick_1, thick_2:thick_2, thick_3:thick_3, $
          thick_4:thick_4, thick_5:thick_5, thick_6:thick_6, mirror_1:mirror_1, $
          mirror_2:mirror_2, mirror_3:mirror_3, mirror_4:mirror_4, mirror_5:mirror_5, $
          mirror_6:mirror_6, angle_1:angle_1, angle_2:angle_2, angle_3:angle_3, $
          angle_4:angle_4, angle_5:angle_5, angle_6:angle_6, filename:filename,  $
          plot_index:plot_index, sbase2a:sbase2a, s_newbase:s_newbase, filterIndex:filterIndex, $
          mirrorIndex:mirrorIndex, s_display:s_display}
Widget_control, show_base, set_uvalue = s, /no_copy
XManager, 'Show', show_base, group_leader = group, event_handler ='Show_event'

end






