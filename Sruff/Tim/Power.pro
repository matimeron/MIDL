;***********Undulator power event handler***********
Pro Power_event, event

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base
common display_info, glob, x, y, out_levs, lin_out, annot_out, xtit, $
                     ytit, ztit, title_out, subtitle_out

Widget_control, event.top, get_uvalue = p, /no_copy
if N_Elements(p.p_cur->Get_Value()) eq 0 then p.p_cur->Set_Value, 0.
if N_Elements(p.p_dist->Get_Value()) eq 0 then p.p_dist->Set_Value, 0
if N_Elements(p.p_k->Get_Value()) eq 0 then p.p_k->Set_Value, 0.
if N_Elements(p.p_lamb->Get_Value()) eq 0 then p.p_lamb->Set_Value, 0.
if N_Elements(p.p_rgam->Get_Value()) eq 0 then p.p_rgam->Set_Value, 0.
if N_Elements(p.p_nper->Get_Value()) eq 0 then p.p_nper->Set_Value, 0
if N_Elements(p.p_sigx->Get_Value()) eq 0 then p.p_sigx->Set_Value, 0.
if N_Elements(p.p_sigy->Get_Value()) eq 0 then p.p_sigy->Set_Value, 0.
if N_Elements(p.p_sigx_prim->Get_Value()) eq 0 then p.p_sigx_prim->Set_Value, 0.
if N_Elements(p.p_sigy_prim->Get_Value()) eq 0 then p.p_sigy_prim->Set_Value, 0.
if N_Elements(p.p_winx->Get_Value()) eq 0 then p.p_winx->Set_Value, 0.
if N_Elements(p.p_winy->Get_Value()) eq 0 then p.p_winy->Set_Value, 0.
miliAmp = p.p_cur->get_value()
dist_p = p.p_dist->get_value()
k_p = p.p_k->get_value()
centi_lamb_p = p.p_lamb->get_value()
rgam_p = p.p_rgam->get_value()
nper_p = p.p_nper->get_value()
sigx = p.p_sigx->get_value()
sigy = p.p_sigy->get_value()
sigx_prim = p.p_sigx_prim->get_value()
sigy_prim = p.p_sigy_prim->get_value()
winx = p.p_winx->get_value()
winy = p.p_winy->get_value()
Widget_control, p.p_smear, get_value = smear_value
Widget_control, p.p_display, get_value = display_choice
Wset, p.p_winIndex
cur_p = miliAmp/1000.
lamb_p = centi_lamb_p / 100.
rsig_p = [sigx, sigy]
asig_p = [sigx_prim, sigy_prim]
quadx = winx/2.
quady = winy/2.
win_p = [quadx, quady]

if smear_value eq 0 then begin
   p.p_nper->SetTabNext, p.p_sigx->GetTextID()
   p.p_sigx->SetTabNext, p.p_sigy->GetTextID()
   p.p_sigy->SetTabNext, p.p_sigx_prim->GetTextID()
   p.p_sigx_prim->SetTabNext, p.p_sigy_prim->GetTextID()
   p.p_sigy_prim->SetTabNext, p.p_winx->GetTextID()
endif

if smear_value eq 1 then begin
   p.p_nper->SetTabNext, p.p_winx->GetTextID()
endif

Widget_control, event.id, get_uvalue = eventval
case eventval of
   "B_CON":begin
      Widget_control, event.id, get_value = bvalue
      if bvalue eq 0 then Widget_control, p.pbase3, /sensitive
      if bvalue eq 1 then Widget_control, p.pbase3, sensitive = 0
      info_1 = p
      endcase

   "CALCULATE1": begin
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy

      info_1 = p
      ;**********Begin input error checking**********
      if miliamp le 0. then begin
         message_out = DIALOG_MESSAGE( ['Storage ring beam current cannot be negative or zero!','',$
                                  'Please enter a value for the beam current (unit = mA).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
     if dist_p le 0. then begin
         message_out = DIALOG_MESSAGE( ['Distance to source cannot be negative or zero!','',$
                                  'Please enter a value for the distance to source (unit = m).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
     if k_p le 0. then begin
         message_out = DIALOG_MESSAGE( ['Deflection parameter k cannot be negative or zero!','',$
                                  'Please enter a value for the deflection parameter.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
     if lamb_p le 0. then begin
         message_out = DIALOG_MESSAGE( ['Undulator period length cannot be negative or zero!','',$
                                  'Please enter a value for the undulator period length (unit = m).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
     if rgam_p le 0. then begin
         message_out = DIALOG_MESSAGE( ['Relativistic gamma cannot be negative or zero!','',$
                                  'Please enter a value for the relativistic gamma parameter.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
     if nper_p le 0. then begin
         message_out = DIALOG_MESSAGE( ['Number of periods cannot be negative or zero!','',$
                                  'Please enter a value for the number of periods.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
     if winx le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture x size cannot be negative or zero!','',$
                                  'Please enter a value for the x size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
     if winy le 0. then begin
         message_out = DIALOG_MESSAGE( ['Aperture y size cannot be negative or zero!','',$
                                  'Please enter a value for the y size.',$
                                  '(Note: full aperture and units = mm)'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = info_1, /no_copy
         endif
     return
     endif
      ;***********End input error checking***********

      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      Widget_control, p.p_display, set_value = 0

      ; smear
      if smear_value eq 0 then begin
         un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, $
         asig_p, /sme, window = win_p, outpower = opow
      endif
      ; no smear
      if smear_value eq 1 then begin
        un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, $
        window = win_p, outpower = opow
      endif

      Widget_control, event.top, /sensitive
      Widget_control, p.base2, sensitive=0
      Widget_control, p.p_output, /sensitive
      Widget_control, p.p_newbase, /sensitive
      Widget_control, p.p_plt_base, /sensitive
      info_1 = {p_cur:p.p_cur, p_dist:p.p_dist, p_k:p.p_k, p_lamb:p.p_lamb,$
        p_rgam:p.p_rgam, p_nper:p.p_nper, p_smear:p.p_smear, pbase3:p.pbase3, $
        p_sigx:p.p_sigx, p_sigy:p.p_sigy, p_sigx_prim:p.p_sigx_prim, $
        p_sigy_prim:p.p_sigy_prim, p_winx:p.p_winx, p_winy:p.p_winy, $
        p_display:p.p_display, p_winIndex:p.p_winIndex, p_output:p.p_output, $
        p_newbase:p.p_newbase, base2:p.base2, p_plt_base:p.p_plt_base, $
        p_outpower:opow}
      endcase

   "DISPLAY1": begin
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
      info_1 = p
    endcase

   "EXITPOWER": begin
      Widget_control, event.top, /DESTROY
      info_1 = p
   endcase

   "FIT1": begin
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      y_n_slice = 0
      depths = ''
      winx_quad = winx/2.
      winy_quad = winy/2.
      Sruff_fit, p.p_outpower, winx_quad, winy_quad, y_n_slice, depths, Group = event.top
      info_1 = p
      endcase

   "HELP1" :begin
     XDisplayFile, "", $
     title = "Help for undulator raw total power and power density calculation", $
     group = event.top, $
     width = 70, $
     height = 42, $
     done_button = "Exit", $
     text =['This routine will calculate undulator raw total power and power density', '', $
            'NOTE: Example values given are for APS undulator A at closed gap.','','','',$
            'Description of input parameters for the undulator raw power calculation:','',$
            '','Storage ring beam current: input unit = mA,  example: 100','',$
            'Distance to source:  input unit = meter,  example: 30','',$
            'Deflection parameter k:  unitless,  example: 2.76  (for undulator A at closed gap)','',$
            'Undulator period length:  input unit = cm,  example: 3.3','',$
            'Relativistic gamma:  unitless,  13700 for APS, depends on storage ring energy','',$
            'Number of periods:  unitless,  example: 72','',$
            'Beam convolution: yes/no choice , to count for the effect of source size and divergence','',$
            'Beam size:  required only if "Beam convolution" is selected,  input unit = millimeter',$
            '                   example:  sigx: 0.352',$
            '                                   sigy: 0.018','',$
            'Beam divergence:  required only if "Beam convolution" is selected,  input unit = mrad',$
            '                              example:  sigx`: 0.022',$
            '                                              sigy`: 0.0042','',$
            'Full aperture:  full aperture size,  input unit = millimeter',$
            '                      example:  xsize: 10',$
            '                                      ysize: 8','',$
            'Display:  select desired display','',$
            'Output options:  select desired output options', '', $
            'Fit to a formula:  activates display to perform gaussian fit to raw power calculation']
   info_1 = p
   endcase

   "NEWCALC1": begin
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      Widget_control, p.base2, /sensitive
      Widget_control, p.p_output, sensitive = 0
      Widget_control, p.p_newbase, sensitive = 0
      Widget_control, p.p_plt_base, sensitive = 0
      info_1 = p
   endcase

   "PRINT1": begin
     printer_choice = dialog_printersetup()
     if printer_choice eq 0 then begin
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = p, /no_copy
        endif
        return
     endif

      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      if smear_value eq 1 then begin
        if display_choice eq 0 then begin
          output, $
          'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, window = win_p',$
           subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p',$
           cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p, $
           DEVICE = 'PRINTER'
        endif
        if display_choice eq 1 then begin
          output, $
          'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, window = win_p, /surf',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p, $
          DEVICE = 'PRINTER'
        endif
        if display_choice eq 2 then begin
          output, $
          'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, window = win_p, /shade',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p, $
          DEVICE = 'PRINTER'
        endif
      endif
      if smear_value eq 0 then begin
        if display_choice eq 0 then begin
          output, "un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p," + $
          " asig_p, /sme, window = win_p",$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p, $
          DEVICE = 'PRINTER'
        endif
        if display_choice eq 1 then begin
          output,$
          'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p,' + $
          ' /sme, window = win_p, /surf',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p, $
          DEVICE = 'PRINTER'
        endif
        if display_choice eq 2 then begin
          output,'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p,' + $
          ' /sme, window = win_p, /shade',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p, $
          DEVICE = 'PRINTER'
        endif
      endif
      Widget_control, event.top, /sensitive
      info_1 = p
   endcase

   "SAVE1":begin
      Widget_control, /hourglass
      Widget_control, event.top, sensitive = 0
      outfile = dialog_pickfile(dialog_parent = p_calculate, get_path = path_p, $
      path = 'C:\my documents',title ='Please Select a Folder and Enter a Filename', /write)
      if outfile eq'' then begin
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = p, /no_copy
         endif
         Widget_control, event.top, /sensitive
         return
      endif

      if smear_value eq 1 then begin
        if display_choice eq 0 then begin
          output,'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, window = win_p',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p, $
          DEVICE = 'EPS', FIL = outfile
        endif
        if display_choice eq 1 then begin
          output, $
          'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, window = win_p, /surf',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p, $
          DEVICE = 'EPS', FIL = outfile
        endif
        if display_choice eq 2 then begin
          output, $
          'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, window = win_p, /shade',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, win_p, $
          DEVICE = 'EPS', FIL = outfile
        endif
      endif
      if smear_value eq 0 then begin
        if display_choice eq 0 then begin
          output, $
          "un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p," + $
          " /sme, window = win_p", $
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p, $
          DEVICE = 'EPS', FIL = outfile
        endif
        if display_choice eq 1 then begin
          output, 'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p,' + $
          ' rsig_p, asig_p, /sme, window = win_p, /surf',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p, $
          DEVICE = 'EPS', FIL = outfile
        endif
        if display_choice eq 2 then begin
          output, 'un_power_ts, cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p,' + $
          ' rsig_p, asig_p, /sme, window = win_p, /shade',$
          subst = 'cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p',$
          cur_p, dist_p, k_p, lamb_p, rgam_p, nper_p, rsig_p, asig_p, win_p, $
          DEVICE = 'EPS', FIL = outfile
        endif
      endif
      Widget_control, event.top, /sensitive
      info_1 = p
   endcase

endcase
if Widget_info(event.top, /valid_id) then begin
   Widget_control, event.top, set_uvalue = info_1, /no_copy
endif
end



;***********Undulator power input parameter display widgets***********
Pro Power, Group = group

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

if (XRegistered( 'Power') ne 0) then return
p_base = Widget_base(title = "UNDULATOR RAW POWER DENSITY CALCULATION", /sensitive, $
                    xoffset = 20, yoffset = 20)
bases = Widget_base(p_base, /row)
pbase = Widget_base(bases, /column)
pbase1 = Widget_base(bases, /column)
pbase2 = Widget_base(pbase, /row)
p_help= Widget_button(pbase2, /align_left, value =' Help ', uvalue ="HELP1")
p_exit = Widget_button(pbase2, /align_left, value =' Exit ', uvalue ="EXITPOWER")
space = Widget_label(pbase, frame = 0, value ='')
space = Widget_label(pbase, frame = 0, value ='')
base2 = Widget_base(pbase, /column, /sensitive)
label = Widget_label(base2, /align_left,  frame = 0, $
        value ='NOTE: DEFAULT PARAMETERS LOADED ARE FOR APS UNDULATOR A  AT')
label = Widget_label(base2, /align_left,  frame = 0, $
        value ='            CLOSED GAP, WITH STANDARD APS STORAGE RING LATTICE' )
space = Widget_label(base2, frame = 0, value ='')
space = Widget_label(base2, frame = 0, value ='')
label = Widget_label(base2, /align_left,  frame = 0, value ='Input Parameters:')
space = Widget_label(base2, frame = 0, value ='')
space = Widget_label(base2, frame = 0, value ='')
p_cur = FSC_Inputfield(base2, title ='Storage ring beam current (mA):',/floatvalue, value = 100)
p_dist = FSC_Inputfield(base2, title ='Distance to source (m):',/floatvalue, value = 30)
p_k = FSC_Inputfield(base2, title ='Deflection parameter k:',/floatvalue, decimal = 3, value = 2.76)
p_lamb = FSC_Inputfield(base2, title ='Undulator period length (cm):', /floatvalue, $
           decimal = 2, value = 3.3)
p_rgam = FSC_Inputfield(base2, title ='Relativistic gamma:', /floatvalue, value = 13700)
p_nper = FSC_Inputfield(base2, title ='Number of periods:', /integervalue, value = 72)
space = Widget_label(base2, frame = 0, value ='')
p_smear = CW_bgroup(base2, /row, label_left='Beam convolution:', /exclusive, /no_release, $
		['Yes', 'No'], uvalue ="B_CON", set_value = 0)
pbase3 = Widget_base(base2, /row, sensitive = 1)
pbase3a = Widget_base(pbase3, /column)
label = Widget_label(pbase3a, /align_left,  frame = 0, value ='Beam size (mm):')
p_sigx = FSC_Inputfield(pbase3a, title ='sigx:', /floatvalue, decimal = 5, value = 0.352)
p_sigy = FSC_Inputfield(pbase3a, title ='sigy:', /floatvalue, decimal = 5, value = 0.018)
pbase3b = Widget_base(pbase3, /column)
label = Widget_label(pbase3b, /align_left,  frame=0, value ='Beam divergence (mrad):')
p_sigx_prim = FSC_Inputfield(pbase3b, title ='sigx`:', /floatvalue, decimal = 5, value = 0.022)
p_sigy_prim = FSC_Inputfield(pbase3b, title ='sigy`:', /floatvalue, decimal = 5, value = 0.0042)
space = Widget_label(base2, frame = 0, value ='')
label = Widget_label(base2, frame = 0, /align_left, value='Full aperture (mm):')
p_winx = FSC_Inputfield(base2, Title='xsize:', /floatvalue, value = 10)
p_winy = FSC_Inputfield(base2, Title='ysize:', /floatvalue, value = 8)
space = Widget_label(base2, frame = 0, value ='')
space = Widget_label(base2, frame = 0, value ='')
space = Widget_label(base2, frame = 0, value ='')
p_calculate = Widget_button(base2, /align_center, value =' CALCULATE ', uvalue ="CALCULATE1")
space = Widget_label(base2, frame = 0, value ='')
space = Widget_label(base2, frame = 0, value ='')
p_plt_base = Widget_base(pbase, frame = 1, sensitive = 0, /column, /align_center)
label = Widget_label(p_plt_base, /align_center,  frame = 0, value ='Display:')
p_display = CW_bgroup(p_plt_base, /row, /exclusive, /no_release, $
                   ['contour', 'surface', 'shaded surface'], uvalue ="DISPLAY1", set_value = 0)
space = Widget_label(pbase, frame = 0, value ='')
space = Widget_label(pbase, frame = 0, value ='')
p_output = Widget_base(pbase, frame = 1, /align_center, sensitive = 0, /column)
label = Widget_label(p_output, /align_center ,  frame = 0, value ='Output options:')
space = Widget_Label(p_output, frame = 0, value ='')
output1 = Widget_base(p_output,/align_center, frame = 0, /row)
p_print = Widget_button(output1, frame = 0, /align_center, value ='         PRINT          ', $
                        uvalue ="PRINT1")
p_save = Widget_button(output1, frame = 0, /align_center, value ='  SAVE AS EPS FILE  ', uvalue ="SAVE1")
space = Widget_label(pbase, frame = 0, value ='')
space = Widget_label(pbase, frame = 0, value ='')
p_newbase = Widget_base(pbase, frame = 0, sensitive = 0, /align_center, /row)
p_fit = Widget_button(p_newbase, frame = 0, value ='    FIT TO A FORMULA    ', uvalue ="FIT1")
p_newcalc = Widget_button(p_newbase, frame = 0, value =' NEW POWER CALCULATION ', $
                        uvalue = "NEWCALC1")
space = Widget_label(pbase, frame = 0, value ='')
space = Widget_label(pbase, frame = 0, value ='')
space = Widget_label(pbase1, frame = 0, value ='')
space = Widget_label(pbase1, frame = 0, value ='')
p_plot = Widget_draw(pbase1, /frame, xsize = 768, ysize = 768)

p_cur->SetTabNext, p_dist->GetTextID()
p_dist->SetTabNext, p_k->GetTextID()
p_k->SetTabNext, p_lamb->GetTextID()
p_lamb->SetTabNext, p_rgam->GetTextID()
p_rgam->SetTabNext, p_nper->GetTextID()
p_nper->SetTabNext, p_sigx->GetTextID()
p_sigx->SetTabNext, p_sigy->GetTextID()
p_sigy->SetTabNext, p_sigx_prim->GetTextID()
p_sigx_prim->SetTabNext, p_sigy_prim->GetTextID()
p_sigy_prim->SetTabNext, p_winx->GetTextID()
p_winx->SetTabNext, p_winy->GetTextID()
p_winy->SetTabNext, p_cur->GetTextID()

Widget_control, p_base, /realize
Widget_control, p_plot, get_value = p_winIndex
p_info = {p_cur:p_cur, p_dist:p_dist, p_k:p_k, p_lamb:p_lamb, p_rgam:p_rgam, $
        p_nper:p_nper, p_smear:p_smear, pbase3:pbase3, p_sigx:p_sigx, p_sigy:p_sigy, $
        p_sigx_prim:p_sigx_prim, p_sigy_prim:p_sigy_prim, p_winx:p_winx, p_winy:p_winy, $
        p_display:p_display, p_winIndex:p_winIndex, p_output:p_output, p_newbase:p_newbase, $
        base2:base2, p_plt_base:p_plt_base}
Widget_control, p_base, set_uvalue = p_info, /no_copy
XManager, 'Power', p_base, group_leader = group, event_handler ='Power_event'

end






