Pro Slice_event, event

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base
common slice_plot_info, farr, x, y, out_levs, lin_out, annot_out, xtit, $  ;T.S 7/21/03
                     ytit, ztit, title_out, subtitle_out   ;T.S 7/21/03

Widget_control, event.top, get_uvalue = sl, /no_copy
Widget_control, event.id, get_uvalue = eventvalue

if N_Elements(sl.sl_cur->Get_Value()) eq 0 then sl.sl_cur->Set_Value, 0.
if N_Elements(sl.sl_dist->Get_Value()) eq 0 then sl.sl_dist->Set_Value, 0.
if N_Elements(sl.sl_harm_num->Get_Value()) eq 0 then sl.sl_harm_num->Set_Value, 0
if N_Elements(sl.sl_sigx->Get_Value()) eq 0 then sl.sl_sigx->Set_Value, 0.
if N_Elements(sl.sl_sigy->Get_Value()) eq 0 then sl.sl_sigy->Set_Value, 0.
if N_Elements(sl.sl_sigx_prim->Get_Value()) eq 0 then sl.sl_sigx_prim->Set_Value, 0.
if N_Elements(sl.sl_sigy_prim->Get_Value()) eq 0 then sl.sl_sigy_prim->Set_Value, 0.
if N_Elements(sl.filter_1->Get_Value()) eq 0 then sl.filter_1->Set_Value, ''
if N_Elements(sl.filter_2->Get_Value()) eq 0 then sl.filter_2->Set_Value, ''
if N_Elements(sl.filter_3->Get_Value()) eq 0 then sl.filter_3->Set_Value, ''
if N_Elements(sl.filter_4->Get_Value()) eq 0 then s.filter_4->Set_Value, ''
if N_Elements(sl.filter_5->Get_Value()) eq 0 then sl.filter_5->Set_Value, ''
if N_Elements(sl.filter_6->Get_Value()) eq 0 then sl.filter_6->Set_Value, ''
if N_Elements(sl.mirror_1->Get_Value()) eq 0 then sl.mirror_1->Set_Value, ''
if N_Elements(sl.mirror_2->Get_Value()) eq 0 then sl.mirror_2->Set_Value, ''
if N_Elements(sl.mirror_3->Get_Value()) eq 0 then sl.mirror_3->Set_Value, ''
if N_Elements(sl.mirror_4->Get_Value()) eq 0 then sl.mirror_4->Set_Value, ''
if N_Elements(sl.mirror_5->Get_Value()) eq 0 then sl.mirror_5->Set_Value, ''
if N_Elements(sl.mirror_6->Get_Value()) eq 0 then sl.mirror_6->Set_Value, ''
if N_Elements(sl.thick_1->Get_Value()) eq 0 then sl.thick_1->Set_Value, 0.
if N_Elements(sl.thick_2->Get_Value()) eq 0 then sl.thick_2->Set_Value, 0.
if N_Elements(sl.thick_3->Get_Value()) eq 0 then sl.thick_3->Set_Value, 0.
if N_Elements(sl.thick_4->Get_Value()) eq 0 then sl.thick_4->Set_Value, 0.
if N_Elements(sl.thick_5->Get_Value()) eq 0 then sl.thick_5->Set_Value, 0.
if N_Elements(sl.thick_6->Get_Value()) eq 0 then sl.thick_6->Set_Value, 0.
if N_Elements(sl.angle_1->Get_Value()) eq 0 then sl.angle_1->Set_Value, 0.
if N_Elements(sl.angle_2->Get_Value()) eq 0 then sl.angle_2->Set_Value, 0.
if N_Elements(sl.angle_3->Get_Value()) eq 0 then sl.angle_3->Set_Value, 0.
if N_Elements(sl.angle_4->Get_Value()) eq 0 then sl.angle_4->Set_Value, 0.
if N_Elements(sl.angle_5->Get_Value()) eq 0 then sl.angle_5->Set_Value, 0.
if N_Elements(sl.angle_6->Get_Value()) eq 0 then sl.angle_6->Set_Value, 0.


miliAmp = sl.sl_cur->get_value()
dist_sl =sl.sl_dist->get_value()
sigx_sl = sl.sl_sigx->get_value()
sigy_sl = sl.sl_sigy->get_value()
sigx_prim_sl = sl.sl_sigx_prim->get_value()
sigy_prim_sl = sl.sl_sigy_prim->get_value()
winx_sl = sl.sl_winx->get_value()
winy_sl = sl.sl_winy->get_value()
f_1 = sl.filter_1->get_value()
f_2 = sl.filter_2->get_value()
f_3 = sl.filter_3->get_value()
f_4 = sl.filter_4->get_value()
f_5 = sl.filter_5->get_value()
f_6 = sl.filter_6->get_value()
t_1 = sl.thick_1->get_value()
t_2 = sl.thick_2->get_value()
t_3 = sl.thick_3->get_value()
t_4 = sl.thick_4->get_value()
t_5 = sl.thick_5->get_value()
t_6 = sl.thick_6->get_value()
m_1 = sl.mirror_1->get_value()
m_2 = sl.mirror_2->get_value()
m_3 = sl.mirror_3->get_value()
m_4 = sl.mirror_4->get_value()
m_5 = sl.mirror_5->get_value()
m_6 = sl.mirror_6->get_value()
a_1 = sl.angle_1->get_value()
a_2 = sl.angle_2->get_value()
a_3 = sl.angle_3->get_value()
a_4 = sl.angle_4->get_value()
a_5 = sl.angle_5->get_value()
a_6 = sl.angle_6->get_value()
name_sl = sl.filename

harms = sl.sl_harm_num->get_value()
separate_slices = strsplit(harms, ',', /extract)
length = n_elements(separate_slices)
harm_slice = fltarr(length)
for i = 0, length-1  do begin
   harm_slice(i) = float(separate_slices(i))
endfor

Widget_control, sl.sl_display, get_value = display_choice

if sl.filterIndex eq 1 then begin
   sl_filts = [f_1]
   sl_filths = [t_1]
endif
if sl.filterIndex eq 2 then begin
   sl_filts = [f_1, f_2]
   sl_filths = [t_1, t_2]
endif
if sl.filterIndex eq 3 then begin
   sl_filts = [f_1, f_2, f_3]
   sl_filths = [t_1, t_2, t_3]
endif
if sl.filterIndex eq 4 then begin
   sl_filts = [f_1, f_2, f_3, f_4]
   sl_filths = [t_1, t_2, t_3, t_4]
endif
if sl.filterIndex eq 5 then begin
   sl_filts = [f_1, f_2, f_3, f_4, f_5]
   sl_filths = [t_1, t_2, t_3, t_4, t_5]
endif
if sl.filterIndex eq 6 then begin
   sl_filts = [f_1, f_2, f_3, f_4, f_5, f_6]
   sl_filths = [t_1, t_2, t_3, t_4, t_5, t_6]
endif
if sl.mirrorIndex eq 1 then begin
   sl_mirrs = [m_1]
   sl_mirans = [a_1]
endif
if sl.mirrorIndex eq 2 then begin
   sl_mirrs = [m_1, m_2]
   sl_mirans = [a_1, a_2]
endif
if sl.mirrorIndex eq 3 then begin
   sl_mirrs = [m_1, m_2, m_3]
   sl_mirans = [a_1, a_2, a_3]
endif
if sl.mirrorIndex eq 4 then begin
   sl_mirrs = [m_1, m_2, m_3, m_4]
   sl_mirans = [a_1, a_2, a_3, a_4]
endif
if sl.mirrorIndex eq 5 then begin
   sl_mirrs = [m_1, m_2, m_3, m_4, m_5]
   sl_mirans = [a_1, a_2, a_3, a_4, a_5]
endif
if sl.mirrorIndex eq 6 then begin
   sl_mirrs = [m_1, m_2, m_3, m_4, m_5, m_6]
   sl_mirans = [a_1, a_2, a_3, a_4, a_5, a_6]
endif

cur_sl = miliAmp/1000.
rsig_sl = [sigx_sl, sigy_sl]
asig_sl = [sigx_prim_sl, sigy_prim_sl]
win_sl = [winx_sl, winy_sl]

Wset, sl.plot_index

case eventvalue of

   "FILTER_NUM": begin
      f_bases = [sl.filter1_base, sl.filter2_base, sl.filter3_base, $
                 sl.filter4_base, sl.filter5_base, sl.filter6_base]

      sl.filterIndex = event.index
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

   endcase

   "MIRROR_NUM": begin
      m_bases = [sl.mirror1_base, sl.mirror2_base, sl.mirror3_base, $
                 sl.mirror4_base, sl.mirror5_base, sl.mirror6_base]

      sl.mirrorIndex = event.index
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
   endcase

   "SL_CALCULATE": begin
      ;**********Begin input error checking**********
      if miliamp le 0. then begin
         message_out = DIALOG_MESSAGE( ['Storage ring beam current cannot be negative or zero!','',$
                                  'Please enter a value for the beam current (unit = mA).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = sl, /no_copy
         endif
     return
     endif
     if dist_sl le 0. then begin
         message_out = DIALOG_MESSAGE( ['Distance to source cannot be negative or zero!','',$
                                  'Please enter a value for the distance to source (unit = m).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = sl, /no_copy
         endif
     return
     endif
     for i = 0, (n_elements(harm_slice) - 1) do begin
        if harm_slice(i) le 0. then begin
           message_out = DIALOG_MESSAGE( ['Harmonic number cannot be negative or zero!','',$
                                  'Please enter a harmonic number.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
           if Widget_info(event.top, /valid_id) then begin
              Widget_control, event.top, set_uvalue = sl, /no_copy
           endif
           return
        endif
     endfor
     if sl.filename eq '' then begin
        message_out = DIALOG_MESSAGE( ['A spectrum file must be selected','',$
                                  'Please select a spectrum file.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = sl, /no_copy
            return
         endif
     endif
     if winx_sl le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture x size cannot be negative or zero!','',$
                                  'Please enter a value for the x size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = sl, /no_copy
         endif
     return
     endif
     if winy_sl le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture y size cannot be negative or zero!','',$
                                  'Please enter a value for the y size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = sl, /no_copy
         endif
     return
     endif
     if sl.filterIndex ne 0 then begin
        for i = 0, sl.filterIndex - 1 do begin
           if sl_filts(i) eq '' then begin
              message_out = DIALOG_MESSAGE( ['Filter material field must contain information','',$
                                  'Please enter the symbol of an element for the filter material.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = sl, /no_copy
              endif
              return
           endif
           if sl_filths(i) le 0 then begin
              message_out = DIALOG_MESSAGE( ['Filter thickness cannot be negative or zero','',$
                                  'Please enter a value for the filter thickness. (unit = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = sl, /no_copy
              endif
              return
           endif
        endfor
     endif
     if sl.mirrorIndex ne 0 then begin
        for i = 0, sl.mirrorIndex - 1 do begin
           if sl_mirrs(i) eq '' then begin
              message_out = DIALOG_MESSAGE( ['Mirror coating field must contain information','',$
                                  'Please enter the symbol of an element for the mirror coating.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = sl, /no_copy
              endif
              return
           endif
           if sl_mirans(i) le 0 then begin
              message_out = DIALOG_MESSAGE( ['Mirror angles cannot be negative or zero','',$
                                  'Please enter a value for the mirror angle. (unit = mrad)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
              if Widget_info(event.top, /valid_id) then begin
                 Widget_control, event.top, set_uvalue = sl, /no_copy
              endif
              return
           endif
        endfor
     endif
     ;***********End input error checking***********
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      Widget_control, sl.sl_display, set_value = 0
      if sl.filterIndex gt 0 then begin
         if sl.mirrorIndex gt 0 then begin
            un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
             slice = harm_slice, /harm_units, /smear, window = win_sl, $
             filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs, $
             mirangles = sl_mirans,  wait = 3
         endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
            slice = harm_slice, /harm_units, /smear, window = win_sl, $
            filters = sl_filts, filthicks = sl_filths, wait = 3
      endif
      if sl.filterIndex eq 0 then begin
         if sl.mirrorIndex gt 0 then begin
            un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
             slice = harm_slice, /harm_units, /smear, window = win_sl, $
             mirrors = sl_mirrs, mirangles = sl_mirans, wait = 3
         endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
            slice = harm_slice, /harm_units, /smear, window = win_sl, $
            wait = 3
      endif
      Widget_control, event.top, /sensitive
      Widget_control, sl.slic_base1b, sensitive = 0
      Widget_control, sl.slic_base2a, /sensitive
      Widget_control, sl.sl_calcbase, sensitive = 0
      Widget_control, sl.sl_newbase, /sensitive
   endcase

   "SL_DISPLAY": begin
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs, $
                 mirangles = sl_mirans, wait = 3
            endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 filters = sl_filts, filthicks = sl_filths, wait = 3
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 mirrors = sl_mirrs, mirangles = sl_mirans, wait = 3
            endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, wait = 3
         endif
      endif
      if display_choice eq 1 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs, $
                 mirangles = sl_mirans, /surf, wait = 3
            endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 filters = sl_filts, filthicks = sl_filths, /surf, wait = 3
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 mirrors = sl_mirrs, mirangles = sl_mirans, /surf, wait = 3
            endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, /surf, $
                 wait = 3
         endif
      endif
      if display_choice eq 2 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs, $
                 mirangles = sl_mirans, /shade, wait = 3
            endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 filters = sl_filts, filthicks = sl_filths, /shade, wait = 3
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, $
                 mirrors = sl_mirrs, mirangles = sl_mirans, /shade, wait = 3
            endif else un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl, $
                 slice = harm_slice, /harm_units, /smear, window = win_sl, /shade, $
                 wait = 3
         endif
      endif
      Widget_control, event.top, /sensitive
   endcase

   "SL_NEWCALC": begin
      Widget_control, sl.slic_base1b, /sensitive
      Widget_control, sl.slic_base2a, sensitive = 0
      Widget_control, sl.sl_calcbase, /sensitive
      Widget_control, sl.sl_newbase, sensitive = 0
   endcase

   "SL_SELECT": begin
      result = un_info_ts(file = '')
     Widget_control, sl.sl_text, set_value = result.result_string
     sl.filename = result.result_string(0)
     temp_winx = result.alims(0) * dist_sl
     temp_winy = result.alims(1) * dist_sl
     sl.sl_winx->set_value, temp_winx
     sl.sl_winy->set_value, temp_winy
   endcase

   "SLICE_EXIT": begin
      Widget_control, event.top, /destroy
      return
   endcase

   "SLICE_HELP": XDisplayFile, "", $
       title = "HARMONIC SIZE AND INTENSITY HELP", $
       group = event.top, $
       width = 70, $
       height = 50, $
       done_button = "Exit", $
       text = ['This routine calculates the photon intensity for a specific energy slice.',$
               'It also calculates the harmonic size.','', '', $
               'NOTE: Default parametern loaded are for APS undulator A','','', $
               'Note: A spectrum file must be selected before calculation can be made.', $
               'Spectrum may be chosen from the existing database or generated using ', $
               'the GENERATE UNDULATOR SPECTRUM routine.','', $
               'Storage ring beam current: input unit = mA,  example: 100','',$
               'Distance to source:  input unit = meter,  example: 30','',$
               'Harmonic energy:  enter energy as a factor of 1st harmonic energy', $
               '                      example: enter 3 for 3rd harmonic energy', $
               '                      OR enter a series of values near a harmonic to find the peak', $
               '                      example enter 0.98, 1.0, 1.02 for E/E1 = 0.98, 1.0, 1.02 respectively.','', $
               'Beam size:  input unit = millimeter',$
               '                   example:  sigx: 0.352',$
               '                                   sigy: 0.018','',$
               'Beam divergence:  input unit = mrad',$
               '                              example:  sigx`: 0.022',$
               '                                              sigy`: 0.0042','',$
               'Aperture size:  quadrant,  input unit = millimeter',$
               '     After a spectrum file is selected, the default aperture is', $
               '     set to the maximum size allowed','',  $
               'Select the appropriate number of filters and mirrors', '', $
               'filter material:  enter an element symbol according to the atomic table', $
               '                      example: c for carbon','',$
               'filter thickness:  input unit = millimeter', $
               '                         example: 0.3', '', $
               'mirror coating:  enter an element symbol according to the atomic table', $
               '                        example: si for silicon','',$
               'mirror angle:  input unit = milliradian', $
               '                     example: 4.0']



   "SL_PRINT": begin
      printer_choice = dialog_printersetup()
      if printer_choice eq 0 then begin
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = sl, /no_copy
         endif
         return
      endif
      name_sl = sl.filename
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs,' + $
                 'mirangles = sl_mirans', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl,harm_slice, win_sl,' + $
                 'sl_filts, sl_filths, sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl,harm_slice, win_sl, $
                 sl_filts, sl_filths, sl_mirrs, sl_mirans, DEVICE = 'PRINTER'
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths', cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, $
                 win_sl, sl_filts, sl_filths, DEVICE = 'PRINTER'
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'mirrors = sl_mirrs, mirangles = sl_mirans' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_mirrs, sl_mirans', cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, $
                 harm_slice, win_sl,csl_mirrs, sl_mirans, DEVICE = 'PRINTER'
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl,' + $
               'file = name_sl, slice = harm_slice, /harm_units, /smear, window = win_sl', $
               sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl', $
               cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
               DEVICE = 'PRINTER'
         endif
      endif
      ;surface plot
      if display_choice eq 1 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs,' + $
                 'mirangles = sl_mirans, /surf', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths, sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, sl_mirrs, sl_mirans, DEVICE = 'PRINTER'
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, /surf', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, DEVICE = 'PRINTER'
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'mirrors = sl_mirrs, mirangles = sl_mirans, /surf', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_mirrs, sl_mirans, DEVICE = 'PRINTER'
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl, /surf' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, DEVICE = 'PRINTER'
         endif
      endif
      ; shaded surface plot
      if display_choice eq 2 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs,' + $
                 'mirangles = sl_mirans, /shade', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths, sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, sl_mirrs, sl_mirans, DEVICE = 'PRINTER'
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, /shade', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, DEVICE = 'PRINTER'
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'mirrors = sl_mirrs, mirangles = sl_mirans, /shade', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_mirrs, sl_mirans, DEVICE = 'PRINTER'
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl, /shade' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, DEVICE = 'PRINTER'
         endif
      endif
      Widget_control, event.top, /sensitive
   endcase

   "SL_SAVE": begin
   message_out = DIALOG_MESSAGE( ['The "SAVE AS EPS FILE" option is currently limited to write', $
                                  'a single file.  As a result, the file written will contain a', $
                                  'plot of the first in a series of harmonic numbers.'], $
                                   DIALOG_PARENT=event.top, /information, title = 'INFORMATION!')
      sl_outfile = dialog_pickfile(dialog_parent = event.top, get_path=path_sl, $
     path = 'C:\my documents',title = 'Please Select a Folder and Enter a Filename', /write)
     if sl_outfile eq '' then begin
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = sl, /no_copy
        endif
        return
     endif
      name_sl = sl.filename
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if display_choice eq 0 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs,' + $
                 'mirangles = sl_mirans', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl,harm_slice, win_sl,' + $
                 'sl_filts, sl_filths, sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl,harm_slice, win_sl, $
                 sl_filts, sl_filths, sl_mirrs, sl_mirans, DEVICE = 'EPS', FIL = sl_outfile
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths', cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, $
                 win_sl, sl_filts, sl_filths, DEVICE = 'EPS', FIL = sl_outfile
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'mirrors = sl_mirrs, mirangles = sl_mirans' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_mirrs, sl_mirans', cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, $
                 harm_slice, win_sl,csl_mirrs, sl_mirans, DEVICE = 'EPS', FIL = sl_outfile
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl,' + $
               'file = name_sl, slice = harm_slice, /harm_units, /smear, window = win_sl', $
               sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl', $
               cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
               DEVICE = 'EPS', FIL = sl_outfile
         endif
      endif
      ;surface plot
      if display_choice eq 1 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs,' + $
                 'mirangles = sl_mirans, /surf', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths, sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, sl_mirrs, sl_mirans, DEVICE = 'EPS', FIL = sl_outfile
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, /surf', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, DEVICE = 'EPS', FIL =  sl_outfile
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'mirrors = sl_mirrs, mirangles = sl_mirans, /surf', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_mirrs, sl_mirans, DEVICE = 'EPS', FIL =  sl_outfile
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl, /surf' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, DEVICE = 'EPS', $
                 FIL = sl_outfile
         endif
      endif
      ; shaded surface plot
      if display_choice eq 2 then begin
         if sl.filterIndex gt 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, mirrors = sl_mirrs,' + $
                 'mirangles = sl_mirans, /shade', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths, sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, sl_mirrs, sl_mirans, DEVICE = 'EPS', FIL = sl_outfile
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'filters = sl_filts, filthicks = sl_filths, /shade', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_filts, sl_filths', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_filts, sl_filths, DEVICE = 'EPS', FIL = sl_outfile
         endif
         if sl.filterIndex eq 0 then begin
            if sl.mirrorIndex gt 0 then begin
               output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl,' + $
                 'mirrors = sl_mirrs, mirangles = sl_mirans, /shade', $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl,' + $
                 'sl_mirrs, sl_mirans', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, $
                 sl_mirrs, sl_mirans, DEVICE = 'EPS', FIL = sl_outfile
            endif else output, 'un_slice_ts, cur_sl, dist_sl, rsig_sl, asig_sl, file = name_sl,' + $
                 'slice = harm_slice, /harm_units, /smear, window = win_sl, /shade' , $
                 sub = 'cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl', $
                 cur_sl, dist_sl, rsig_sl, asig_sl, name_sl, harm_slice, win_sl, DEVICE = 'EPS', $
                 FIL = sl_outfile
         endif
      endif
      Widget_control, event.top, /sensitive
   endcase
endcase

f =[sl.filter_1,sl.thick_1,sl.filter_2,sl.thick_2,sl.filter_3,sl.thick_3, $
   sl.filter_4,sl.thick_4,sl.filter_5,sl.thick_5,sl.filter_6,sl.thick_6]

m =[sl.mirror_1, sl.angle_1, sl.mirror_2, sl.angle_2, sl.mirror_3, sl.angle_3, $
    sl.mirror_4, sl.angle_4, sl.mirror_5, sl.angle_5, sl.mirror_6, sl.angle_6]

fIndex = 2*sl.filterIndex
mIndex = 2*sl.mirrorIndex

if (fIndex eq 0) and (mIndex eq 0) then begin
   sl.sl_harm_num->SetTabNext, sl.sl_sigx->GetTextID()
endif
if (fIndex eq 0) and (mIndex gt 0) then begin
   sl.sl_harm_num->SetTabNext, m(0)->GetTextID()
   for j = 0, (mIndex - 2) do begin
      m(j)->SetTabNext, m(j+1)->GetTextID()
   endfor
   m(mIndex - 1)->SetTabNext, sl.sl_sigx->GetTextID()
endif
if (fIndex gt 0) and  (mIndex eq 0) then begin
   sl.sl_harm_num->SetTabNext, f(0)->GetTextID()
   for i = 0, (fIndex -2 ) do begin
      f(i)->SetTabNext, f(i+1)->GetTextID()
   endfor
   f(fIndex - 1)->SetTabNext, sl.sl_sigx->GetTextID()
endif
if (fIndex gt 0) and (mIndex gt 0) then begin
   sl.sl_harm_num->SetTabNext, f(0)->GetTextID()
   for i = 0, (fIndex - 2) do begin
      f(i)->SetTabNext, f(i+1)->GetTextID()
   endfor
   f(fIndex - 1)->SetTabNext, m(0)->GetTextID()
   for j = 0, (mIndex - 2) do begin
      m(j)->SetTabNext, m(j+1)->GetTextID()
   endfor
   m(mIndex - 1)->SetTabNext, sl.sl_sigx->GetTextID()
endif

if Widget_info(event.top, /valid_id) then begin
        Widget_control, event.top, set_uvalue = sl, /no_copy
endif
end



Pro Slice, Group = group

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

if (XRegistered( 'Slice') ne 0) then return

slice_base = Widget_base(title = "HARMONIC SIZE AND INTENSITY CALCULATION", xoffset = 20, $
                        yoffset = 20, /row)
slic_base1 = Widget_base(slice_base, /column)
slic_base2 = Widget_base(slice_base, /column)
slic_base1a = Widget_base(slic_base1, /row)
sl_help= Widget_button(slic_base1a, /align_left, value =' Help ', uvalue ="SLICE_HELP")
sl_exit = Widget_button(slic_base1a, /align_left, value =' Exit ', uvalue ="SLICE_EXIT")
label = Widget_label(slic_base1a, /align_left,  frame = 0, $
        value ='     NOTE: DEFAULT PARAMETERS LOADED ARE FOR APS UNDULATOR A' )
slic_base1b = Widget_base(slic_base1, /column)
space = Widget_label(slic_base1b, frame = 0, value ='')
slic_base1b1 = Widget_base(slic_base1b, /row)
sl_cur = FSC_Inputfield(slic_base1b1, title ='Storage ring beam current (mA):', $
                      /column, /floatvalue, value = 100)
space = Widget_label(slic_base1b1, frame = 0, value ='      ')
sl_dist = FSC_Inputfield(slic_base1b1, title ='Distance to source (m):', $
                      /column, /floatvalue, value = 30)
sl_harm_num = FSC_Inputfield(slic_base1b, value = '1', $
   title ='Enter energy as a factor of 1st harmonic energy (3 for 3rd harmonic energy):')
harm_label = Widget_label(slic_base1b, frame = 0, /align_left,  $
   value = '(to find the peak intensity near a harmonic, enter a series of values separated by commas)')
space = Widget_label(slic_base1b, frame = 0, value ='')
slic_base1b2 = Widget_base(slic_base1b, /row)
slic_base1b2a = Widget_base(slic_base1b2, /column)
sl_file = Widget_button(slic_base1b2a,  frame = 0, /align_center, value ='SELECT SPECTRUM FILE', $
          uvalue ="SL_SELECT")
space = Widget_label(slic_base1b2a, frame = 0, value ='')
sl_text = Widget_text(slic_base1b2a, frame = 0, value = '', xsize = 45, ysize = 16)
slic_base1b2b = Widget_base(slic_base1b2, /column)
label = Widget_label(slic_base1b2b, /align_left,  frame = 0, value ='Beam size (mm):')
sl_sigx = FSC_Inputfield(slic_base1b2b, title ='sigx:', /floatvalue, decimal = 5, value = 0.352)
sl_sigy = FSC_Inputfield(slic_base1b2b, title ='sigy:', /floatvalue, decimal = 5, value = 0.018)
space = Widget_label(slic_base1b2b, frame = 0, value ='')
label = Widget_label(slic_base1b2b, /align_left,  frame=0, value ='Beam divergence (mrad):')
sl_sigx_prim = FSC_Inputfield(slic_base1b2b, title ='sigx`:', /floatvalue, decimal = 5, value = 0.022)
sl_sigy_prim = FSC_Inputfield(slic_base1b2b, title ='sigy`:', /floatvalue, decimal = 5, value = 0.0042)
space = Widget_label(slic_base1b2b, frame = 0, value ='')
label = Widget_label(slic_base1b2b, frame = 0, /align_left, value='Aperture size (quadrant, mm):')
sl_winx = FSC_Inputfield(slic_base1b2b, Title='xsize:', /floatvalue, value = 0, decimal = 2)
sl_winy = FSC_Inputfield(slic_base1b2b, Title='ysize:', /floatvalue, value = 0, decimal = 2)
optics_base = Widget_base(slic_base1b, /align_left, /column)
label = Widget_label(optics_base, frame = 0, /align_left, $
           value ='Parameters of filters and mirrors:')
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
sl_plot = Widget_draw(slic_base2, /frame, xsize = 730, ysize = 730)
space = Widget_label(slic_base2, frame = 0, value ='')
space = Widget_label(slic_base2, frame = 0, value ='')
sl_calcbase = Widget_base(slic_base2, frame = 0, /row, /align_center)
sl_calculate = Widget_button(sl_calcbase, /align_center, value ='           CALCULATE          ', $
                            uvalue ="SL_CALCULATE")
slic_base2a = Widget_base(slic_base2, frame = 0, sensitive = 0, /row, /align_center)

plot_base = Widget_base(slic_base2a, frame = 1, /column, /align_center)
label = Widget_label(plot_base, /align_center,  frame = 0, value ='Display:')
sl_display = CW_bgroup(plot_base, /row, /exclusive, /no_release, $
                   ['contour', 'surface', 'shaded surface'], uvalue ="SL_DISPLAY", set_value = 0)
space = Widget_label(slic_base2a, frame = 0, value ='     ')
output_base = Widget_base(slic_base2a, frame = 1, /align_center, /column)
label = Widget_label(output_base, /align_center ,  frame = 0, value ='Output options:')
space = Widget_Label(output_base, frame = 0, value ='')
output1 = Widget_base(output_base,/align_center, frame = 0, /row)
sl_print = Widget_button(output1, frame = 0, /align_center, value ='        PRINT         ', $
                        uvalue ="SL_PRINT")
sl_save = Widget_button(output1, frame = 0, /align_center, value =' SAVE AS EPS FILE ', uvalue ="SL_SAVE")
space = Widget_label(slic_base2, frame = 0, value ='')
sl_newbase = Widget_base(slic_base2, frame = 0, sensitive = 0, /align_center, /row)
sl_newcalc = Widget_button(sl_newbase, frame = 0, value =' NEW HARMONIC SIZE AND INTENSITY CALCULATION ', $
                        uvalue = "SL_NEWCALC")

sl_cur->SetTabNext, sl_dist->GetTextID()
sl_dist->SetTabNext, sl_harm_num->GetTextID()
sl_harm_num->SetTabNext, sl_sigx->GetTextID()
sl_sigx->SetTabNext, sl_sigy->GetTextID()
sl_sigy->SetTabNext, sl_sigx_prim->GetTextID()
sl_sigx_prim->SetTabNext, sl_sigy_prim->GetTextID()
sl_sigy_prim->SetTabNext, sl_winx->GetTextID()
sl_winx->SetTabNext, sl_winy->GetTextID()
sl_winy->SetTabNext, sl_cur->GetTextID()

Widget_control, slice_base, /realize
Widget_control, sl_plot, get_value = plot_index
filterIndex = 0
mirrorIndex = 0
filename = ''

sl = {slic_base1b:slic_base1b, sl_cur:sl_cur, sl_dist:sl_dist, sl_harm_num:sl_harm_num, $
      sl_text:sl_text, sl_sigx:sl_sigx, sl_sigy:sl_sigy, sl_sigx_prim:sl_sigx_prim, $
      sl_sigy_prim:sl_sigy_prim, sl_winx:sl_winx, sl_winy:sl_winy, filter_number:filter_number, $
      mirror_number:mirror_number, filter1_base:filter1_base, filter2_base:filter2_base, $
      filter3_base:filter3_base, filter4_base:filter4_base, filter5_base:filter5_base, $
      filter6_base:filter6_base, mirror1_base:mirror1_base, mirror2_base:mirror2_base, $
      mirror3_base:mirror3_base, mirror4_base:mirror4_base, mirror5_base:mirror5_base, $
      mirror6_base:mirror6_base, filter_1:filter_1, filter_2:filter_2, filter_3:filter_3, $
      filter_4:filter_4, filter_5:filter_5, filter_6:filter_6, thick_1:thick_1, thick_2:thick_2, $
      thick_3:thick_3, thick_4:thick_4, thick_5:thick_5, thick_6:thick_6, mirror_1:mirror_1, $
      mirror_2:mirror_2, mirror_3:mirror_3, mirror_4:mirror_4, mirror_5:mirror_5, mirror_6:mirror_6, $
      angle_1:angle_1, angle_2:angle_2, angle_3:angle_3, angle_4:angle_4, angle_5:angle_5, $
      angle_6:angle_6, filename:filename, plot_index:plot_index, slic_base2a:slic_base2a, $
      sl_newbase:sl_newbase, filterIndex:filterIndex, mirrorIndex:mirrorIndex, sl_display:sl_display, $
      sl_calcbase:sl_calcbase}
Widget_control, slice_base, set_uvalue = sl, /no_copy
XManager, 'Slice', slice_base, group_leader = group, event_handler ='Slice_event'

end