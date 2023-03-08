Pro Absorb_event, event

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

Widget_control, event.top, get_uvalue = a, /no_copy
Widget_control, event.id, get_uvalue = eventvalue

if N_Elements(a.a_cur->Get_Value()) eq 0 then a.a_cur->Set_Value, 0.
if N_Elements(a.a_dist->Get_Value()) eq 0 then a.a_dist->Set_Value, 0.
if N_Elements(a.a_sigx->Get_Value()) eq 0 then a.a_sigx->Set_Value, 0.
if N_Elements(a.a_sigy->Get_Value()) eq 0 then a.a_sigy->Set_Value, 0.
if N_Elements(a.a_sigx_prim->Get_Value()) eq 0 then a.a_sigx_prim->Set_Value, 0.
if N_Elements(a.a_sigy_prim->Get_Value()) eq 0 then a.a_sigy_prim->Set_Value, 0.
if N_Elements(a.filter_1->Get_Value()) eq 0 then a.filter_1->Set_Value, ''
if N_Elements(a.filter_2->Get_Value()) eq 0 then a.filter_2->Set_Value, ''
if N_Elements(a.filter_3->Get_Value()) eq 0 then a.filter_3->Set_Value, ''
if N_Elements(a.filter_4->Get_Value()) eq 0 then s.filter_4->Set_Value, ''
if N_Elements(a.filter_5->Get_Value()) eq 0 then a.filter_5->Set_Value, ''
if N_Elements(a.filter_6->Get_Value()) eq 0 then a.filter_6->Set_Value, ''
if N_Elements(a.mirror_1->Get_Value()) eq 0 then a.mirror_1->Set_Value, ''
if N_Elements(a.mirror_2->Get_Value()) eq 0 then a.mirror_2->Set_Value, ''
if N_Elements(a.mirror_3->Get_Value()) eq 0 then a.mirror_3->Set_Value, ''
if N_Elements(a.mirror_4->Get_Value()) eq 0 then a.mirror_4->Set_Value, ''
if N_Elements(a.mirror_5->Get_Value()) eq 0 then a.mirror_5->Set_Value, ''
if N_Elements(a.mirror_6->Get_Value()) eq 0 then a.mirror_6->Set_Value, ''
if N_Elements(a.thick_1->Get_Value()) eq 0 then a.thick_1->Set_Value, 0.
if N_Elements(a.thick_2->Get_Value()) eq 0 then a.thick_2->Set_Value, 0.
if N_Elements(a.thick_3->Get_Value()) eq 0 then a.thick_3->Set_Value, 0.
if N_Elements(a.thick_4->Get_Value()) eq 0 then a.thick_4->Set_Value, 0.
if N_Elements(a.thick_5->Get_Value()) eq 0 then a.thick_5->Set_Value, 0.
if N_Elements(a.thick_6->Get_Value()) eq 0 then a.thick_6->Set_Value, 0.
if N_Elements(a.angle_1->Get_Value()) eq 0 then a.angle_1->Set_Value, 0.
if N_Elements(a.angle_2->Get_Value()) eq 0 then a.angle_2->Set_Value, 0.
if N_Elements(a.angle_3->Get_Value()) eq 0 then a.angle_3->Set_Value, 0.
if N_Elements(a.angle_4->Get_Value()) eq 0 then a.angle_4->Set_Value, 0.
if N_Elements(a.angle_5->Get_Value()) eq 0 then a.angle_5->Set_Value, 0.
if N_Elements(a.angle_6->Get_Value()) eq 0 then a.angle_6->Set_Value, 0.
if N_Elements(a.a_absorber->Get_Value()) eq 0 then a.a_absorber->Set_Value, ''
if N_Elements(a.a_tilt->Get_Value()) eq 0 then a.a_tilt->Set_Value, 90.
if N_Elements(a.a_depth->Get_Value()) eq 0 then a.a_depth->Set_Value, '0.0'

miliAmp = a.a_cur->get_value()
dist_a =a.a_dist->get_value()
sigx_a = a.a_sigx->get_value()
sigy_a = a.a_sigy->get_value()
sigx_prim_a = a.a_sigx_prim->get_value()
sigy_prim_a = a.a_sigy_prim->get_value()
winx_a = a.a_winx->get_value()
winy_a = a.a_winy->get_value()
f_1 = a.filter_1->get_value()
f_2 = a.filter_2->get_value()
f_3 = a.filter_3->get_value()
f_4 = a.filter_4->get_value()
f_5 = a.filter_5->get_value()
f_6 = a.filter_6->get_value()
t_1 = a.thick_1->get_value()
t_2 = a.thick_2->get_value()
t_3 = a.thick_3->get_value()
t_4 = a.thick_4->get_value()
t_5 = a.thick_5->get_value()
t_6 = a.thick_6->get_value()
m_1 = a.mirror_1->get_value()
m_2 = a.mirror_2->get_value()
m_3 = a.mirror_3->get_value()
m_4 = a.mirror_4->get_value()
m_5 = a.mirror_5->get_value()
m_6 = a.mirror_6->get_value()
a_1 = a.angle_1->get_value()
a_2 = a.angle_2->get_value()
a_3 = a.angle_3->get_value()
a_4 = a.angle_4->get_value()
a_5 = a.angle_5->get_value()
a_6 = a.angle_6->get_value()
absorber_a = a.a_absorber->get_value()
tilt_a = a.a_tilt->get_value()
name_a = a.filename

depths = a.a_depth->get_value()
separate_depths = strsplit(depths, ',', /extract)
length = n_elements(separate_depths)
depth_a = fltarr(length)
for i = 0, length-1  do begin
   depth_a(i) = float(separate_depths(i))
endfor

Widget_control, a.a_display, get_value = display_choice

if a.filterIndex eq 1 then begin
   a_filts = [f_1]
   a_filths = [t_1]
endif
if a.filterIndex eq 2 then begin
   a_filts = [f_1, f_2]
   a_filths = [t_1, t_2]
endif
if a.filterIndex eq 3 then begin
   a_filts = [f_1, f_2, f_3]
   a_filths = [t_1, t_2, t_3]
endif
if a.filterIndex eq 4 then begin
   a_filts = [f_1, f_2, f_3, f_4]
   a_filths = [t_1, t_2, t_3, t_4]
endif
if a.filterIndex eq 5 then begin
   a_filts = [f_1, f_2, f_3, f_4, f_5]
   a_filths = [t_1, t_2, t_3, t_4, t_5]
endif
if a.filterIndex eq 6 then begin
   a_filts = [f_1, f_2, f_3, f_4, f_5, f_6]
   a_filths = [t_1, t_2, t_3, t_4, t_5, t_6]
endif

if a.mirrorIndex eq 1 then begin
   a_mirrs = [m_1]
   a_mirans = [a_1]
endif
if a.mirrorIndex eq 2 then begin
   a_mirrs = [m_1, m_2]
   a_mirans = [a_1, a_2]
endif
if a.mirrorIndex eq 3 then begin
   a_mirrs = [m_1, m_2, m_3]
   a_mirans = [a_1, a_2, a_3]
endif
if a.mirrorIndex eq 4 then begin
   a_mirrs = [m_1, m_2, m_3, m_4]
   a_mirans = [a_1, a_2, a_3, a_4]
endif
if a.mirrorIndex eq 5 then begin
   a_mirrs = [m_1, m_2, m_3, m_4, m_5]
   a_mirans = [a_1, a_2, a_3, a_4, a_5]
endif
if a.mirrorIndex eq 6 then begin
   a_mirrs = [m_1, m_2, m_3, m_4, m_5, m_6]
   a_mirans = [a_1, a_2, a_3, a_4, a_5, a_6]
endif


cur_a = miliAmp/1000.
rsig_a = [sigx_a, sigy_a]
asig_a = [sigx_prim_a, sigy_prim_a]
win_a = [winx_a, winy_a]

Wset, a.plot_index

case eventvalue of

   "FILTER_NUM": begin
      f_bases = [a.filter1_base, a.filter2_base, a.filter3_base, $
                 a.filter4_base, a.filter5_base, a.filter6_base]

      a.filterIndex = event.index
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
      new_info = a
   endcase

   "MIRROR_NUM": begin
      m_bases = [a.mirror1_base, a.mirror2_base, a.mirror3_base, $
                 a.mirror4_base, a.mirror5_base, a.mirror6_base]

      a.mirrorIndex = event.index
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
      new_info = a
   endcase

   "A_CALCULATE": begin
      new_info = a
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
     if dist_a le 0. then begin
         message_out = DIALOG_MESSAGE( ['Distance to source cannot be negative or zero!','',$
                                  'Please enter a value for the distance to source (unit = m).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif
     if a.filename eq '' then begin
        message_out = DIALOG_MESSAGE( ['A spectrum file must be selected','',$
                                  'Please select a spectrum file.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif

     if winx_a le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture x size cannot be negative or zero!','',$
                                  'Please enter a value for the x size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif
     if winy_a le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture y size cannot be negative or zero!','',$
                                  'Please enter a value for the y size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = new_info, /no_copy
         endif
     return
     endif
     if absorber_a eq '' then begin
        message_out = DIALOG_MESSAGE( ['Absorber material field must contain information','',$
                      'Please enter the symbol of an element for the absorber material.'], $
                      DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = new_info, /no_copy
        endif
        return
     endif
     if tilt_a eq 0. then begin
        message_out = DIALOG_MESSAGE( ['Absorber tilt angle cannot be zero','',$
                      'Please enter a value for the absorber tilt angle.', $
                      '(Note: units = degrees and angle is 90 degrees for absorber surface', $
                      'perpendicular to beam)'], $
                      DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = new_info, /no_copy
        endif
        return
     endif
     for i = 0, (n_elements(depth_a) - 1) do begin
        if depth_a(i) le 0. then begin
            message_out = DIALOG_MESSAGE( ['Absorber depth cannot be negative or zero','',$
                         'Please enter the absorber thickness or a series of depths.', $
                         '(Note: units = mm)'], $
                         DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
           if Widget_info(event.top, /valid_id) then begin
              Widget_control, event.top, set_uvalue = new_info, /no_copy

           endif
           return
        endif
     endfor
     if a.filterIndex ne 0 then begin
        for i = 0, a.filterIndex - 1 do begin
           if a_filts(i) eq '' then begin
              message_out = DIALOG_MESSAGE( ['Filter material field must contain information','',$
                                  'Please enter the symbol of an element for the filter material.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = new_info, /no_copy
              endif
              return
           endif
           if a_filths(i) le 0 then begin
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
     if a.mirrorIndex ne 0 then begin
        for i = 0, a.mirrorIndex - 1 do begin
           if a_mirrs(i) eq '' then begin
              message_out = DIALOG_MESSAGE( ['Mirror coating field must contain information','',$
                                  'Please enter the symbol of an element for the mirror coating.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = new_info, /no_copy
              endif
              return
           endif
           if a_mirans(i) le 0 then begin
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
      Widget_control, a.a_display, set_value = 0
      if a.filterIndex gt 0 then begin
         if a.mirrorIndex gt 0 then begin
            un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                 /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                 filters = a_filts, filthicks = a_filths, mirrors = a_mirrs, $
                 mirangles = a_mirans, absorber = absorber_a, depths = depth_a, $
                 /show, wait = 3, outdata = opow_a
         endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                 /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                 filters = a_filts, filthicks = a_filths, absorber = absorber_a, $
                 depths = depth_a, /show, wait = 3, outdata = opow_a
      endif
      if a.filterIndex eq 0 then begin
         if a.mirrorIndex gt 0 then begin
            un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                 /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                 mirrors = a_mirrs, mirangles = a_mirans, absorber = absorber_a, $
                 depths = depth_a, /show, wait = 3, outdata = opow_a
         endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                 /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                 absorber = absorber_a, depths = depth_a, /show, wait = 3, outdata = opow_a
      endif


      Widget_control, event.top, /sensitive
      Widget_control, a.abase1b, sensitive = 0
      Widget_control, a.a_calcbase, sensitive = 0
      Widget_control, a.abase2a, /sensitive
      Widget_control, a.a_newbase, /sensitive

      new_info = {abase1b:a.abase1b, a_cur:a.a_cur, a_dist:a.a_dist, a_text:a.a_text, $
       a_sigx:a.a_sigx, a_sigy:a.a_sigy, a_sigx_prim:a.a_sigx_prim, a_sigy_prim:a.a_sigy_prim, $
       a_winx:a.a_winx, a_winy:a.a_winy, filter_number:a.filter_number, $
       mirror_number:a.mirror_number, filter1_base:a.filter1_base, filter2_base:a.filter2_base, $
       filter3_base:a.filter3_base, filter4_base:a.filter4_base, filter5_base:a.filter5_base, $
       filter6_base:a.filter6_base, mirror1_base:a.mirror1_base, mirror2_base:a.mirror2_base, $
       mirror3_base:a.mirror3_base, mirror4_base:a.mirror4_base, mirror5_base:a.mirror5_base, $
       mirror6_base:a.mirror6_base, filter_1:a.filter_1, filter_2:a.filter_2, filter_3:a.filter_3, $
       filter_4:a.filter_4, filter_5:a.filter_5, filter_6:a.filter_6, thick_1:a.thick_1, $
       thick_2:a.thick_2, thick_3:a.thick_3, thick_4:a.thick_4, thick_5:a.thick_5, thick_6:a.thick_6, $
       mirror_1:a.mirror_1, mirror_2:a.mirror_2, mirror_3:a.mirror_3, mirror_4:a.mirror_4, $
       mirror_5:a.mirror_5, mirror_6:a.mirror_6, angle_1:a.angle_1, angle_2:a.angle_2, $
       angle_3:a.angle_3, angle_4:a.angle_4, angle_5:a.angle_5, angle_6:a.angle_6, $
       filename:a.filename, plot_index:a.plot_index, a_calcbase:a.a_calcbase, abase2a:a.abase2a, $
       a_newbase:a.a_newbase, filterIndex:a.filterIndex, mirrorIndex:a.mirrorIndex, $
       a_display:a.a_display, a_absorber:a.a_absorber, a_tilt:a.a_tilt, a_depth:a.a_depth, $
       a_outpower:opow_a}
   endcase

   "A_DISPLAY": begin
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    filters = a_filts, filthicks = a_filths, mirrors = a_mirrs, $
                    mirangles = a_mirans, absorber = absorber_a, depths = depth_a, $
                    /show, wait = 3
            endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    filters = a_filts, filthicks = a_filths, absorber = absorber_a, $
                    depths = depth_a, /show, wait = 3
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    mirrors = a_mirrs, mirangles = a_mirans, absorber = absorber_a, $
                    depths = depth_a, /show, wait = 3
            endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    absorber = absorber_a, depths = depth_a, /show, wait = 3
         endif
      endif
      ;surface plot
      if display_choice eq 1 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    filters = a_filts, filthicks = a_filths, mirrors = a_mirrs, $
                    mirangles = a_mirans, absorber = absorber_a, depths = depth_a, $
                    /show, wait = 3, /surf
            endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    filters = a_filts, filthicks = a_filths, absorber = absorber_a, $
                    depths = depth_a, /show, wait = 3, /surf
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    mirrors = a_mirrs, mirangles = a_mirans, absorber = absorber_a, $
                    depths = depth_a, /show, wait = 3, /surf
            endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    absorber = absorber_a, depths = depth_a, /show, wait = 3, /surf
         endif
      endif
      ; shaded surface plot
      if display_choice eq 2 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    filters = a_filts, filthicks = a_filths, mirrors = a_mirrs, $
                    mirangles = a_mirans, absorber = absorber_a, depths = depth_a, $
                    /show, wait = 3, /shade
            endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    filters = a_filts, filthicks = a_filths, absorber = absorber_a, $
                    depths = depth_a, /show, wait = 3, /shade
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    mirrors = a_mirrs, mirangles = a_mirans, absorber = absorber_a, $
                    depths = depth_a, /show, wait = 3, /shade
            endif else un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a, $
                    /smear, window = win_a, tilt_ang = tilt_a, /degrees, $
                    absorber = absorber_a, depths = depth_a, /show, wait = 3, /shade
         endif
      endif
      new_info = a
      Widget_control, event.top, /sensitive
   endcase

   "A_NEWCALC": begin
      Widget_control, a.abase1b, /sensitive
      Widget_control, a.a_calcbase, /sensitive
      Widget_control, a.abase2a, sensitive = 0
      Widget_control, a.a_newbase, sensitive = 0
      new_info = a
   endcase

   "A_SELECT": begin
      result = un_info_ts(file = '')
     Widget_control, a.a_text, set_value = result.result_string
     a.filename = result.result_string(0)
     temp_winx = result.alims(0) * dist_a
     temp_winy = result.alims(1) * dist_a
     a.a_winx->set_value, temp_winx
     a.a_winy->set_value, temp_winy
     new_info = a
   endcase

   "A_EXIT": begin
      Widget_control, event.top, /destroy
      return
   endcase

   "A_FIT": begin
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      y_n_slice = 1
      slice_choice = strarr(length)
      for i = 0, length - 1 do begin
         if i eq 0 then begin
            temp = string('0.0 -  ' + separate_depths(i) + ' mm')
         endif else begin
            temp = string(separate_depths(i-1) + ' - ' + separate_depths(i) + ' mm')
         endelse
         slice_choice(i) = temp
      endfor
      Sruff_fit, a.a_outpower, winx_a, winy_a, y_n_slice, slice_choice, Group = event.top
      new_info = a
      endcase

   "A_HELP": begin
       XDisplayFile, "", $
       title = "POWER ABSORBTION HELP", $
       group = event.top, $
       width = 70, $
       height = 50, $
       done_button = "Exit", $
       text = ['This routine calculates the absorbed power density in w/mm^3 ', $
               'in thin layers of filters and windows','', $
               'NOTE: Default parameters loaded are for APS undulator A','', $
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
               'Aperture size:  input unit = millimeter', $
               '   The maximum aperture size is restricted by the aperture size in the spectrum file.', $
               '   After a spectrum file is selected, the  default aperture size is set to the ', $
               '   maximum size allowed.','', $
               'Absorber material:  enter an element symbol according to the atomic table','',$
               'Incident angle:  input unit = degrees',$
               '     Note: absorber surface perpendicular to beam is 90 degrees','',$
               'Depths: thickness of absorber or depths of a series of slices', $
               '               input unit = millimeter','',$
               'Select the appropriate number of filters and mirrors upstream of absorber', '', $
               'filter material:  enter an element symbol according to the atomic table', $
               '                      example: c for carbon','',$
               'filter thickness:  input unit = millimeter', $
               '                         example: 0.3', '', $
               'mirror coating:  enter an element symbol according to the atomic table', $
               '                        example: si for silicon','',$
               'mirror angle:  input unit = milliradian', $
               '                     example: 4.0']
      new_info = a
   endcase

   "A_PRINT": begin
      printer_choice = dialog_printersetup()
      if printer_choice eq 0 then begin
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = a, /no_copy
         endif
         return
      endif
      name_a = a.filename
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               output,'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
               '/smear, window = win_a, tilt_ang = tilt_a, /degrees, filters = a_filts,' + $
               'filthicks = a_filths, mirrors = a_mirrs, mirangles = a_mirans,' + $
               'absorber = absorber_a, depths = depth_a, /show', sub = 'cur_a,' + $
               'dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths,' + $
               'a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, a_filts, a_filths, a_mirrs, a_mirans, absorber_a, $
                depth_a, DEVICE = 'PRINTER'
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a,' + $
              'file = name_a, /smear, window = win_a, tilt_ang = tilt_a, /degrees,' + $
              'filters = a_filts, filthicks = a_filths, absorber = absorber_a,' + $
              'depths = depth_a, /show', sub = 'cur_a, dist_a, rsig_a,' + $
              'asig_a, name_a, win_a, tilt_a, a_filts, a_filths, absorber_a, depth_a', $
              cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths, $
              absorber_a, depth_a, DEVICE = 'PRINTER'
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, mirrors = a_mirrs,' + $
                'mirangles = a_mirans, absorber = absorber_a, depths = depth_a, /show', $
                 sub = 'cur_a, dist_a, rsig_a, asig_a, name_a, win_a,' + $
                'tilt_a, a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a,$
                 asig_a, name_a, win_a, tilt_a, a_mirrs, a_mirans, absorber_a, depth_a, $
                 DEVICE = 'PRINTER'
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, absorber = absorber_a,' + $
                'depths = depth_a, /show', sub = 'cur_a, dist_a, rsig_a, asig_a,' + $
                'name_a, win_a, tilt_a, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, absorber_a, depth_a, DEVICE = 'PRINTER'
         endif
      endif
      ;surface plot
      if display_choice eq 1 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               output,'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
               '/smear, window = win_a, tilt_ang = tilt_a, /degrees, filters = a_filts,' + $
               'filthicks = a_filths, mirrors = a_mirrs, mirangles = a_mirans,' + $
               'absorber = absorber_a, depths = depth_a, /show, /surf', sub = 'cur_a,' + $
               'dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths,' + $
               'a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, $
               asig_a, name_a, win_a, tilt_a, a_filts, a_filths, a_mirrs, a_mirans, $
               absorber_a, depth_a, DEVICE = 'PRINTER'
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a,' + $
              'file = name_a, /smear, window = win_a, tilt_ang = tilt_a, /degrees,' + $
              'filters = a_filts, filthicks = a_filths, absorber = absorber_a,' + $
              'depths = depth_a, /show, /surf', sub = 'cur_a, dist_a, rsig_a, asig_a,' +$
              'name_a, win_a, tilt_a, a_filts, a_filths, absorber_a, depth_a', $
              cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, $
              a_filths, absorber_a, depth_a, DEVICE = 'PRINTER'
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, mirrors = a_mirrs,' + $
                'mirangles = a_mirans, absorber = absorber_a, depths = depth_a,  /show, /surf', $
                sub = 'cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_mirrs,' + $
                ' a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, name_a, $
                win_a, tilt_a, a_mirrs, a_mirans, absorber_a, depth_a, DEVICE = 'PRINTER'
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, absorber = absorber_a,' + $
                'depths = depth_a, /show, /surf', sub = 'cur_a, dist_a, rsig_a, asig_a,' + $
                'name_a, win_a, tilt_a, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, absorber_a, depth_a, DEVICE = 'PRINTER'
         endif
      endif

      ; shaded surface plot
      if display_choice eq 2 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               output,'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
               '/smear, window = win_a, tilt_ang = tilt_a, /degrees, filters = a_filts,' + $
               'filthicks = a_filths, mirrors = a_mirrs, mirangles = a_mirans,' + $
               'absorber = absorber_a, depths = depth_a, /show, /shade', sub = 'cur_a,' + $
               'dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths,' + $
               'a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, $
               asig_a, name_a, win_a, tilt_a, a_filts, a_filths, a_mirrs, a_mirans, $
               absorber_a, depth_a, DEVICE = 'PRINTER'
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a,' + $
              'file = name_a, /smear, window = win_a, tilt_ang = tilt_a, /degrees,' + $
              'filters = a_filts, filthicks = a_filths, absorber = absorber_a,' + $
              'depths = depth_a, /show, /shade', sub = 'cur_a, dist_a, rsig_a, asig_a,' +$
              'name_a, win_a, tilt_a, a_filts, a_filths, absorber_a, depth_a', $
              cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, $
              a_filths, absorber_a, depth_a, DEVICE = 'PRINTER'
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, mirrors = a_mirrs,' + $
                'mirangles = a_mirans, absorber = absorber_a, depths = depth_a, /show, /shade', $
                sub = 'cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_mirrs,' + $
                ' a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, name_a, $
                win_a, tilt_a, a_mirrs, a_mirans, absorber_a, depth_a, DEVICE = 'PRINTER'
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, absorber = absorber_a,' + $
                'depths = depth_a, /show, /shade', sub = 'cur_a, dist_a, rsig_a, asig_a,' + $
                'name_a, win_a, tilt_a, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, absorber_a, depth_a, DEVICE = 'PRINTER'
         endif
      endif
      new_info = a
      Widget_control, event.top, /sensitive
   endcase

   "A_SAVE": begin
   message_out = DIALOG_MESSAGE( ['The "SAVE AS EPS FILE" option is currently limited to write', $
                                  'a single file.  As a result, the file written will contain a', $
                                  'plot of the first slice of a multiply sliced absorber only.', $
                                  'To generate an EPS file of the energy absorbed in a particular', $
                                  'slice (other than the first slice), enter the thickness', $
                                  'preceeding the desired slice as a filter.'], $
                                   DIALOG_PARENT=event.top, /information, title = 'INFORMATION!')
      a_outfile = dialog_pickfile(dialog_parent = event.top, get_path=path_a, $
     path = 'C:\my documents',title = 'Please Select a Folder and Enter a Filename', /write)
     if a_outfile eq '' then begin
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = a, /no_copy
        endif
        return
     endif
      name_a = a.filename
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               output,'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
               '/smear, window = win_a, tilt_ang = tilt_a, /degrees, filters = a_filts,' + $
               'filthicks = a_filths, mirrors = a_mirrs, mirangles = a_mirans,' + $
               'absorber = absorber_a, depths = depth_a, /show', sub = 'cur_a,' + $
               'dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths,' + $
               'a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, a_filts, a_filths, a_mirrs, a_mirans, absorber_a, $
                depth_a, DEVICE = 'EPS', FIL = a_outfile
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a,' + $
              'file = name_a, /smear, window = win_a, tilt_ang = tilt_a, /degrees,' + $
              'filters = a_filts, filthicks = a_filths, absorber = absorber_a,' + $
              'depths = depth_a, /show', sub = 'cur_a, dist_a, rsig_a,' + $
              'asig_a, name_a, win_a, tilt_a, a_filts, a_filths, absorber_a, depth_a', $
              cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths, $
              absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, mirrors = a_mirrs,' + $
                'mirangles = a_mirans, absorber = absorber_a, depths = depth_a, /show',  $
                 sub = 'cur_a, dist_a, rsig_a, asig_a, name_a, win_a,' + $
                'tilt_a, a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a,$
                 asig_a, name_a, win_a, tilt_a, a_mirrs, a_mirans, absorber_a, depth_a, $
                 DEVICE = 'EPS', FIL = a_outfile
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, absorber = absorber_a,' + $
                'depths = depth_a, /show', sub = 'cur_a, dist_a, rsig_a, asig_a,' + $
                'name_a, win_a, tilt_a, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
         endif
      endif
      ;surface plot
      if display_choice eq 1 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               output,'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
               '/smear, window = win_a, tilt_ang = tilt_a, /degrees, filters = a_filts,' + $
               'filthicks = a_filths, mirrors = a_mirrs, mirangles = a_mirans,' + $
               'absorber = absorber_a, depths = depth_a, /show, /surf', sub = 'cur_a,' + $
               'dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths,' + $
               'a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, $
               asig_a, name_a, win_a, tilt_a, a_filts, a_filths, a_mirrs, a_mirans, $
               absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a,' + $
              'file = name_a, /smear, window = win_a, tilt_ang = tilt_a, /degrees,' + $
              'filters = a_filts, filthicks = a_filths, absorber = absorber_a,' + $
              'depths = depth_a, /show, /surf', sub = 'cur_a, dist_a, rsig_a, asig_a,' +$
              'name_a, win_a, tilt_a, a_filts, a_filths, absorber_a, depth_a', $
              cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, $
              a_filths, absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, mirrors = a_mirrs,' + $
                'mirangles = a_mirans, absorber = absorber_a, depths = depth_a, /show, /surf', $
                sub = 'cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_mirrs,' + $
                ' a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, name_a, $
                win_a, tilt_a, a_mirrs, a_mirans, absorber_a, depth_a, DEVICE = 'EPS', $
                FIL = a_outfile
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, absorber = absorber_a,' + $
                'depths = depth_a, /show, /surf', sub = 'cur_a, dist_a, rsig_a, asig_a,' + $
                'name_a, win_a, tilt_a, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
         endif
      endif
      ; shaded surface plot
      if display_choice eq 2 then begin
         if a.filterIndex gt 0 then begin
            if a.mirrorIndex gt 0 then begin
               output,'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
               '/smear, window = win_a, tilt_ang = tilt_a, /degrees, filters = a_filts,' + $
               'filthicks = a_filths, mirrors = a_mirrs, mirangles = a_mirans,' + $
               'absorber = absorber_a, depths = depth_a, /show, /shade', sub = 'cur_a,' + $
               'dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, a_filths,' + $
               'a_mirrs, a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, $
               asig_a, name_a, win_a, tilt_a, a_filts, a_filths, a_mirrs, a_mirans, $
               absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a,' + $
              'file = name_a, /smear, window = win_a, tilt_ang = tilt_a, /degrees,' + $
              'filters = a_filts, filthicks = a_filths, absorber = absorber_a,' + $
              'depths = depth_a, /show, /shade', sub = 'cur_a, dist_a, rsig_a, asig_a,' +$
              'name_a, win_a, tilt_a, a_filts, a_filths, absorber_a, depth_a', $
              cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_filts, $
              a_filths, absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
         endif
         if a.filterIndex eq 0 then begin
            if a.mirrorIndex gt 0 then begin
               output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, mirrors = a_mirrs,' + $
                'mirangles = a_mirans, absorber = absorber_a, depths = depth_a, /show, /shade', $
                sub = 'cur_a, dist_a, rsig_a, asig_a, name_a, win_a, tilt_a, a_mirrs,' + $
                ' a_mirans, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, name_a, $
                win_a, tilt_a, a_mirrs, a_mirans, absorber_a, depth_a, DEVICE = 'EPS', $
                FIL = a_outfile
            endif else output, 'un_absorb_ts, cur_a, dist_a, rsig_a, asig_a, file = name_a,' + $
                '/smear, window = win_a, tilt_ang = tilt_a, /degrees, absorber = absorber_a,' + $
                'depths = depth_a, /show, /shade', sub = 'cur_a, dist_a, rsig_a, asig_a,' + $
                'name_a, win_a, tilt_a, absorber_a, depth_a', cur_a, dist_a, rsig_a, asig_a, $
                name_a, win_a, tilt_a, absorber_a, depth_a, DEVICE = 'EPS', FIL = a_outfile
         endif
      endif
      new_info = a
      Widget_control, event.top, /sensitive
   endcase
endcase

f =[a.filter_1, a.thick_1, a.filter_2, a.thick_2, a.filter_3, a.thick_3, $
    a.filter_4, a.thick_4, a.filter_5, a.thick_5, a.filter_6, a.thick_6]

m =[a.mirror_1, a.angle_1, a.mirror_2, a.angle_2, a.mirror_3, a.angle_3, $
    a.mirror_4, a.angle_4, a.mirror_5, a.angle_5, a.mirror_6, a.angle_6]

fIndex = 2*a.filterIndex
mIndex = 2*a.mirrorIndex

if (fIndex eq 0) and (mIndex eq 0) then begin
   a.a_depth->SetTabNext, a.a_cur->GetTextID()
endif

if (fIndex eq 0) and (mIndex gt 0) then begin
   a.a_depth->SetTabNext, m(0)->GetTextID()
   for j = 0, (mIndex - 2) do begin
      m(j)->SetTabNext, m(j+1)->GetTextID()
   endfor
   m(mIndex - 1)->SetTabNext, a.a_cur->GetTextID()
endif

if (fIndex gt 0) and  (mIndex eq 0) then begin
   a.a_depth->SetTabNext, f(0)->GetTextID()
   for i = 0, (fIndex -2 ) do begin
      f(i)->SetTabNext, f(i+1)->GetTextID()
   endfor
   f(fIndex - 1)->SetTabNext, a.a_cur->GetTextID()
endif

if (fIndex gt 0) and (mIndex gt 0) then begin
   a.a_depth->SetTabNext, f(0)->GetTextID()
   for i = 0, (fIndex - 2) do begin
      f(i)->SetTabNext, f(i+1)->GetTextID()
   endfor
   f(fIndex - 1)->SetTabNext, m(0)->GetTextID()
   for j = 0, (mIndex - 2) do begin
      m(j)->SetTabNext, m(j+1)->GetTextID()
   endfor
   m(mIndex - 1)->SetTabNext, a.a_cur->GetTextID()
endif

if Widget_info(event.top, /valid_id) then begin
        Widget_control, event.top, set_uvalue = new_info, /no_copy
endif
end



Pro Absorb, Group = group

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

if (XRegistered( 'Absorb') ne 0) then return

absorb_base = Widget_base(title = "POWER ABSORBTION CALCULATION", xoffset = 5, yoffset = 5, /row)
abase1 = Widget_base(absorb_base, /column)
abase2 = Widget_base(absorb_base, /column)
abase1a = Widget_base(abase1, /row)
a_help= Widget_button(abase1a, /align_left, value =' Help ', uvalue ="A_HELP")
s_exit = Widget_button(abase1a, /align_left, value =' Exit ', uvalue ="A_EXIT")
space = Widget_label(abase1a, frame = 0, value ='   ')
label = Widget_label(abase1a, /align_left,  frame = 0, $
        value ='NOTE: DEFAULT PARAMETERS LOADED ARE FOR APS UNDULATOR A' )
abase1b = Widget_base(abase1, /column)
;space = Widget_label(abase1b, frame = 0, value ='')
abase1b1 = Widget_base(abase1b, /row)
a_cur = FSC_Inputfield(abase1b1, title ='Storage ring beam current (mA):', $
                      /column, /floatvalue, value = 100)
space = Widget_label(abase1b1, frame = 0, value ='      ')
a_dist = FSC_Inputfield(abase1b1, title ='Distance to source (m):', $
                      /column, /floatvalue, value = 30)
abase1b2 = Widget_base(abase1b, /row)
abase1b2a = Widget_base(abase1b2, /column)
a_file = Widget_button(abase1b2a,  frame = 0, /align_center, value ='SELECT SPECTRUM FILE', uvalue ="A_SELECT")
;space = Widget_label(abase1b2a, frame = 0, value ='')
a_text = Widget_text(abase1b2a, frame = 0, value = '', xsize = 45, ysize = 16)
abase1b2b = Widget_base(abase1b2, /column)
label = Widget_label(abase1b2b, /align_left,  frame = 0, value ='Beam size (mm):')
a_sigx = FSC_Inputfield(abase1b2b, title ='sigx:', /floatvalue, decimal = 5, value = 0.352)
a_sigy = FSC_Inputfield(abase1b2b, title ='sigy:', /floatvalue, decimal = 5, value = 0.018)
;space = Widget_label(abase1b2b, frame = 0, value ='')
label = Widget_label(abase1b2b, /align_left,  frame=0, value ='Beam divergence (mrad):')
a_sigx_prim = FSC_Inputfield(abase1b2b, title ='sigx`:', /floatvalue, decimal = 5, value = 0.022)
a_sigy_prim = FSC_Inputfield(abase1b2b, title ='sigy`:', /floatvalue, decimal = 5, value = 0.0042)
;space = Widget_label(abase1b2b, frame = 0, value ='')
label = Widget_label(abase1b2b, frame = 0, /align_left, value='Aperture size (quadrant, mm):')
a_winx = FSC_Inputfield(abase1b2b, Title='xsize:', /floatvalue, value = 0, decimal = 1)
a_winy = FSC_Inputfield(abase1b2b, Title='ysize:', /floatvalue, value = 0, decimal = 1)
abase1b1 = Widget_base(abase1b, /align_center, /column, /frame)
label = Widget_label(abase1b1, frame = 0, value = 'Absorber parameters:')
abase1b1a = Widget_base(abase1b1, /align_center, /row)
a_absorber = FSC_Inputfield(abase1b1a, Title='Material:', value = '')
a_tilt = FSC_Inputfield(abase1b1a, Title='Incident angle (degrees):', value = 90., /floatvalue, $
                        decimal = 1)
abase1b1b = Widget_base(abase1b1, /align_center, /row)
a_depth = FSC_Inputfield(abase1b1b, Title='Depth/s (single depth or series separated by commas, unit=mm):', $
                         value = '0.0')
optics_base = Widget_base(abase1b, /align_left, /column)
label = Widget_label(optics_base, frame = 0, /align_left, $
           value ='Parameters of filters and mirrors upstream of absorber:')
number_base = Widget_base(optics_base, frame = 0, /row)
filter_number = Widget_droplist(number_base, title = 'Select number of filters:', $
                                uvalue = "FILTER_NUM", value = ['0','1','2','3','4', '5', '6'])
space = Widget_label(number_base, frame = 0, value ='     ')
mirror_number = Widget_droplist(number_base, title = 'Select number of mirrors:', $
               uvalue = "MIRROR_NUM", value = ['0','1','2','3','4', '5', '6'])
filter1_base = Widget_base(optics_base, /align_left, /row, map = 0)
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
space = Widget_label(abase2, frame = 0, value ='')
a_plot = Widget_draw(abase2, /frame, xsize = 730, ysize = 730)
space = Widget_label(abase2, frame = 0, value ='')
a_calcbase = Widget_base(abase2, /row, /align_center, /sensitive)
a_calculate = Widget_button(a_calcbase, /align_center, value ='           CALCULATE          ', $
                            uvalue ="A_CALCULATE")
space = Widget_label(abase2, frame = 0, value ='')
abase2a = Widget_base(abase2, frame = 0, sensitive = 0, /row, /align_center)
plot_base = Widget_base(abase2a, frame = 1, /column, /align_center)
label = Widget_label(plot_base, /align_center,  frame = 0, value ='Display:')
a_display = CW_bgroup(plot_base, /row, /exclusive, /no_release, $
                   ['contour', 'surface', 'shaded surface'], uvalue ="A_DISPLAY", set_value = 0)
space = Widget_label(abase2a, frame = 0, value ='     ')
output_base = Widget_base(abase2a, frame = 1, /align_center, /column)
label = Widget_label(output_base, /align_center ,  frame = 0, value ='Output options:')
space = Widget_Label(output_base, frame = 0, value ='')
output1 = Widget_base(output_base,/align_center, frame = 0, /row)
a_print = Widget_button(output1, frame = 0, /align_center, value ='         PRINT          ', $
                        uvalue ="A_PRINT")
a_save = Widget_button(output1, frame = 0, /align_center, value ='  SAVE AS EPS FILE  ', uvalue ="A_SAVE")
space = Widget_label(abase2, frame = 0, value ='')
a_newbase = Widget_base(abase2, frame = 0, sensitive = 0, /align_center, /row)
a_fit = Widget_button(a_newbase, frame = 0, value ='    FIT TO A FORMULA    ', uvalue ="A_FIT")
a_newcalc = Widget_button(a_newbase, frame = 0, value =' NEW POWER ABSORBTION CALCULATION ', $
                        uvalue = "A_NEWCALC")

a_cur->SetTabNext, a_dist->GetTextID()
a_dist->SetTabNext, a_sigx->GetTextID()
a_sigx->SetTabNext, a_sigy->GetTextID()
a_sigy->SetTabNext, a_sigx_prim->GetTextID()
a_sigx_prim->SetTabNext, a_sigy_prim->GetTextID()
a_sigy_prim->SetTabNext, a_winx->GetTextID()
a_winx->SetTabNext, a_winy->GetTextID()
a_winy->SetTabNext, a_absorber->GetTextID()
a_absorber->SetTabNext, a_tilt->GetTextID()
a_tilt->SetTabNext, a_depth->GetTextID()
a_depth->SetTabNext, a_cur->GetTextID()

Widget_control, absorb_base, /realize
Widget_control, a_plot, get_value = plot_index
filterIndex = 0
mirrorIndex = 0
filename = ''

a = {abase1b:abase1b, a_cur:a_cur, a_dist:a_dist, a_text:a_text, a_sigx:a_sigx, a_sigy:a_sigy, $
          a_sigx_prim:a_sigx_prim, a_sigy_prim:a_sigy_prim, a_winx:a_winx, a_winy:a_winy, $
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
          plot_index:plot_index, a_calcbase:a_calcbase, abase2a:abase2a, a_newbase:a_newbase, $
          filterIndex:filterIndex, mirrorIndex:mirrorIndex, a_display:a_display, $
          a_absorber:a_absorber, a_tilt:a_tilt, a_depth:a_depth}
Widget_control, absorb_base, set_uvalue = a, /no_copy
XManager, 'Absorb', absorb_base, group_leader = group, event_handler ='Absorb_event'

end





