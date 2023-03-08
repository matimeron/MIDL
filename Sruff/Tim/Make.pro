Pro Make_event, event

common un_consts, hcove, alpha, oovee, ecf
common output_make, display, m_outfile

u = un_struct()

Widget_control, event.top, get_uvalue = m, /no_copy
if N_Elements(m.m_k->Get_Value()) eq 0 then m.m_k->Set_Value, 0.
if N_Elements(m.m_efirst->Get_Value()) eq 0 then m.m_efirst->Set_Value, 0.
if N_Elements(m.m_nharms->Get_Value()) eq 0 then m.m_nharms->Set_Value, ''
if N_Elements(m.m_lamb->Get_Value()) eq 0 then m.m_lamb->Set_Value, 0.
if N_Elements(m.m_rgam->Get_Value()) eq 0 then m.m_rgam->Set_Value, 0.
if N_Elements(m.m_nper->Get_Value()) eq 0 then m.m_nper->Set_Value, 0
if N_Elements(m.m_sigx->Get_Value()) eq 0 then m.m_sigx->Set_Value, 0.
if N_Elements(m.m_sigy->Get_Value()) eq 0 then m.m_sigy->Set_Value, 0.
if N_Elements(m.m_sigx_prim->Get_Value()) eq 0 then m.m_sigx_prim->Set_Value, 0.
if N_Elements(m.m_sigy_prim->Get_Value()) eq 0 then m.m_sigy_prim->Set_Value, 0.
if N_Elements(m.m_ang_h->Get_Value()) eq 0 then m.m_ang_h->Set_Value, 0.
if N_Elements(m.m_ang_v->Get_Value()) eq 0 then m.m_ang_v->Set_Value, 0.
if N_Elements(m.gscl_1->Get_Value()) eq 0 then m.gscl_1->Set_Value, 0.
if N_Elements(m.gscl_2->Get_Value()) eq 0 then m.gscl_2->Set_Value, 0.
if N_Elements(m.m_xpt->Get_Value()) eq 0 then m.m_xpt->Set_Value, 0
if N_Elements(m.m_ypt->Get_Value()) eq 0 then m.m_ypt->Set_Value, 0
k = m.m_k->get_value()
efi = m.m_efirst->get_value()
harms_value = m.m_nharms->get_value()
cm_lamb = m.m_lamb->get_value()
rgam = m.m_rgam->get_value()
nper = m.m_nper->get_value()
sigx = m.m_sigx->get_value()
sigy = m.m_sigy->get_value()
sigx_prim = m.m_sigx_prim->get_value()
sigy_prim = m.m_sigy_prim->get_value()
h_ang = m.m_ang_h->get_value()
v_ang = m.m_ang_v->get_value()
gscl_1 = m.gscl_1->get_value()
gscl_2 = m.gscl_2->get_value()
xpt = m.m_xpt->get_value()
ypt = m.m_ypt->get_value()
nam = m.m_filename->get_value()
Widget_control, m.k_choice, get_value = choice_k
Widget_control, m.gscl_chc, get_value = gscl_chc

lamb = cm_lamb/100.
rsig = [sigx, sigy]
asig = [sigx_prim, sigy_prim]
if gscl_chc eq 0 then als = [h_ang, v_ang]
if gscl_chc eq 1 then als = [gscl_1, gscl_2]
np = [xpt, ypt]

if harms_value ne '' then harms = harms_value

if (choice_k eq 0)then begin
   m.m_k->SetTabNext, m.m_nharms->GetTextID()
   m.m_filename->SetTabNext, m.m_k->GetTextID()
endif

if (choice_k eq 1) then begin
   m.m_efirst->SetTabNext, m.m_nharms->GetTextID()
   m.m_filename->SetTabNext, m.m_efirst->GetTextID()
endif

if (gscl_chc eq 0) then begin
   m.m_sigy_prim->SetTabNext, m.m_ang_h->GetTextID()
   m.m_ang_h->SetTabNext, m.m_ang_v->GetTextID()
   m.m_ang_v->SetTabNext, m.m_xpt->GetTextID()
endif

if (gscl_chc eq 1) then begin
   m.m_sigy_prim->SetTabNext, m.gscl_1->GetTextID()
   m.gscl_1->SetTabNext, m.gscl_2->GetTextID()
   m.gscl_2->SetTabNext, m.m_xpt->GetTextID()
endif

display = m.display

Widget_control, event.id, get_uvalue = eventval

case eventval of

   "A_OR_NUM":begin
      if gscl_chc eq 0 then begin
         Widget_control, m.choice_1a, /sensitive
         Widget_control, m.choice_1b, sensitive = 0
      endif
      if gscl_chc eq 1 then begin
         Widget_control, m.choice_1a, sensitive = 0
         Widget_control, m.choice_1b, /sensitive
      endif
      if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
      endif
      return
   endcase

   "K_OR_E": begin
      if choice_k eq 0 then begin
         m.m_efirst->Set_Value, 0, /floatvalue
         Widget_control, m.c_1a, /sensitive
         Widget_control, m.c_1b, sensitive = 0
      endif
      if choice_k eq 1 then begin
         Widget_control, m.c_1a, sensitive = 0
         Widget_control, m.c_1b, /sensitive
      endif
      if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
      endif
      return
   endcase

   "M_CALC": begin
      ;***********BEGIN INPUT ERROR CHECKING***********
      if k le 0. or k eq '' then begin
         message_out = DIALOG_MESSAGE( ['Deflection parameter k cannot be negative or zero!','',$
                                  'Please enter a value for the deflection parameter.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if choice_k eq 1 and efi le 0. then begin
        message_out = DIALOG_MESSAGE( ['Energy of first harmonic energy cannot be negative or zero!','',$
                                  'Please enter a value for the first harmonic energy.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if harms_value ne '' then begin
        if harms_value le 0. then begin
           message_out = DIALOG_MESSAGE( ['Number of harmonics cannot be negative or zero!','',$
                        'Please enter a value for the number of harmonics.'], $
                         DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
           if Widget_info(event.top, /valid_id) then begin
              Widget_control, event.top, set_uvalue = m, /no_copy
           endif
       return
       endif
     endif
     if lamb le 0. then begin
         message_out = DIALOG_MESSAGE( ['Undulator period length cannot be negative or zero!','',$
                                  'Please enter a value for the undulator period length (unit = m).'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if rgam le 0. then begin
         message_out = DIALOG_MESSAGE( ['Relativistic gamma cannot be negative or zero!','',$
                                  'Please enter a value for the relativistic gamma parameter.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if nper le 0. then begin
         message_out = DIALOG_MESSAGE( ['Number of periods cannot be negative or zero!','',$
                                  'Please enter a value for the number of periods.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if gscl_chc eq 1 and gscl_1 le 0. then begin
         message_out = DIALOG_MESSAGE( ['Horizontal # cannot be negative or zero!','',$
                                  'Please enter a value for the horizontal #.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if gscl_chc eq 1 and gscl_2 le 0. then begin
         message_out = DIALOG_MESSAGE( ['Vertical # cannot be negative or zero!','',$
                                  'Please enter a vertical #.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if xpt le 0. then begin
         message_out = DIALOG_MESSAGE( ['Number of x points cannot be negative or zero!','',$
                                  'Please enter a value for the number of x points.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     if ypt le 0. then begin
         message_out = DIALOG_MESSAGE( ['Number of y points cannot be negative or zero!','',$
                                  'Please enter a value for the number of y points.'], $
                                   DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
     return
     endif
     ;***********END INPUT ERROR CHECKING**********

      Widget_control, /hourglass
      efimax = 2*ecf*hcove*rgam*rgam/lamb          ;cut and pasted out of un_make
      if efi gt 0 then begin
	     if efi le efimax then begin                ;cut and pasted out of un_make
	        kk = sqrt(2*(efimax/efi - 1))          ;cut and pasted out of un_make
		 endif else begin
	     message_out = DIALOG_MESSAGE( 'First harmonic energy cannot exceed ' + $
                        string(efimax, form ='(f8.3)') + ' keV', $
                        DIALOG_PARENT=event.top, /ERROR, title = 'WARNING!')
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
         return
         endelse
      endif

      line1:
      m_outfile = dialog_pickfile(dialog_parent = m_calc, file = nam,  /write, $
      path = 'C:\my documents',title = 'Please Select a Folder and Enter a Filename')
      if m_outfile eq'' then begin
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
         endif
         return
      endif

      file_exist = file_test(m_outfile + '.dat', /NOEXPAND_PATH)

      if file_exist eq 1 then begin
         overwrite = DIALOG_MESSAGE( ['A file of the specified directory and name exists. ', $
                                      'The existing file will be overwritten.','', $
                                      'Do you want to overwrite the existing file?'], $
                                      DIALOG_PARENT=event.top, /question, title = 'WARNING!')
         if overwrite eq 'No' then goto, line1
      endif

      Widget_control, m.display, set_value = ['Progress:', '', $
        'harmonic    time elapsed              harmonic     time remaining',$
        'CALCULATING FIRST HARMONIC']
      Widget_control, m.m_base, sensitive = 0
      if gscl_chc eq 0 then begin
         if choice_k eq 0 then begin
            if harms_value eq '' then begin
               un_make_ts, k, lamb, rgam, nper, nam, rsig, asig,  ang_lims = als, npoints = np, $
                /show_progress
            endif else un_make_ts, k, lamb, rgam, nper, nam, rsig, asig, harmonics = harms, $
              ang_lims = als, npoints = np, /show_progress
         endif
         if choice_k eq 1 then begin
            if harms_value eq '' then begin
               un_make_ts, 0., lamb, rgam, nper, nam, rsig, asig, efirst = efi, $
                  ang_lims = als, npoints = np, /show_progress
            endif else un_make_ts, 0., lamb, rgam, nper, nam, rsig, asig, efirst = efi, $
                  harmonics = harms, ang_lims = als, npoints = np, /show_progress
         endif
      endif
      if gscl_chc eq 1 then begin
         if choice_k eq 0 then begin
            if harms_value eq ''  then begin
               un_make_ts, k, lamb, rgam, nper, nam, rsig, asig, ang_lims = als, $
                 npoints = np, /gscale, /show_progress
            endif else un_make_ts, k, lamb, rgam, nper, nam, rsig, asig, harmonics = harms, $
               ang_lims = als, npoints = np, /gscale, /show_progress
         endif
         if choice_k eq 1 then begin
            if harms_value eq '' then begin
               un_make_ts, 0., lamb, rgam, nper, nam, rsig, asig, efirst = efi, $
                  ang_lims = als, npoints = np, /gscale, /show_progress
            endif else un_make_ts, 0., lamb, rgam, nper, nam, rsig, asig, efirst = efi, $
                  harmonics = harms, ang_lims = als, npoints = np, /gscale, /show_progress
         endif
      endif

      Widget_control, display, get_value = temp
      Widget_control, display, $
       set_value = [temp,'UNDULATOR SPECTRUM CALCULATIONS ARE COMPLETE!']
      Widget_control, m.m_base, /sensitive
   endcase

   "M_EFIRST": begin
      efimax = 2*ecf*hcove*rgam*rgam/lamb          ;cut and pasted out of un_make
      if efi gt 0 then begin
	     if efi le efimax then begin                ;cut and pasted out of un_make
	        kk = sqrt(2*(efimax/efi - 1))
	        m.m_k->Set_Value, kk, /floatvalue
	        h_angle = kk/(1e-3*rgam)
            m.m_ang_h->Set_Value, h_angle, /floatvalue
            e_name = string(efi)
            f_name = STRCOMPRESS( e_name , /REMOVE_ALL )
            split = STRSPLIT(f_name, '.', /extract)
            f_name = 'u_e'+strjoin(split, 'p')
            while strlen(f_name) eq (STRPOS(f_name, '0', /REVERSE_SEARCH)+1) do begin
               f_name = STRMID(f_name, 0, (strlen(f_name) - 1))
            endwhile
            m.m_filename->Set_Value, f_name
		 endif
      endif

      if Widget_info(event.top, /valid_id) then begin
         Widget_control, event.top, set_uvalue = m, /no_copy
      endif
      return
   endcase

   "M_EXIT": begin
      Widget_control, event.top, /destroy
   endcase

   "M_HELP": XDisplayFile, "", $
     title = "Help for undulator spectrum calculation", $
     group = event.top, $
     width = 75, $
     height = 45, $
     done_button = "Exit", $
     text = ['This routine generates an undulator spectrum data structure which is needed', $
         'for absorbtion and transmission calculations', '', '', $
         'Description of input parameters for the undulator spectrum calculation:','','',$
         'Deflection parameter k:  unitless,  example: 2.76  (for APS undulator A at closed gap)','',$
         'Enter first harmonic energy:  input unit = KeV',$
         '       Note: if a value is entered then k will be calculated','',$
         'Number of harmonics to calculate:  integer number of harmonics to calculate  ',$
         '       default is all harmonics when field is left blank', '',$
         'Undulator period length:  input unit = cm,  example: 3.3','',$
         'Relativistic gamma:  unitless,  13700 for APS, depends on storage ring energy','',$
         'Number of periods:  unitless,  example: 72','',$
         'Beam size:  input unit = mm,  example:  sigx: 0.352  sigy: 0.018','',$
         'Beam divergence:  input unit = mrad,  example:  sigx`: 0.022  sigy`: 0.0042', '',$
         'Angular range:   input unit = mrad,', $
         '       defaults:   horizontal = 1e3*[k/gamma]', $
         '                       vertical = 1e3*[1/gamma]','', $
         'Enter a number in units of 1/gamma.  for example, enter 2 for 2/gamma','',$
         'Number of points in quadrant: determines grid density in x and y',$
         '       example:  x points: 80      y points: 40','',$
         'Filename: suggested format is either:', $
         '   u_kxpx where:',$
         '      - "k" denotes calculation based on deflection parameter k', $
         '      - "xpx" is the deflection parameter with the "x" denoting numbers and p denoting',$
         '        the decimal point', $
         '   u_expx where:', $
         '      - "e" denotes calcuation based on first harmonic energy', $
         '      - "xpx" is the first harmonic energy with "x" denoting numbers and p denoting', $
         '        the decimal point']

   "M_K": begin
      h_angle = k/(1e-3*rgam)
      m.m_ang_h->Set_Value, h_angle, /floatvalue
      k_name = string(k)
      f_name = STRCOMPRESS( k_name , /REMOVE_ALL )
      split = STRSPLIT(f_name, '.', /extract)
      f_name = 'u_k'+strjoin(split, 'p')
      while strlen(f_name) eq (STRPOS(f_name, '0', /REVERSE_SEARCH)+1) do begin
         f_name = STRMID(f_name, 0, (strlen(f_name) - 1))
      endwhile
      m.m_filename->Set_Value, f_name
      if Widget_info(event.top, /valid_id) then begin
         Widget_control, event.top, set_uvalue = m, /no_copy
      endif
      return
   endcase

   "M_RGAM": begin
      h_angle = k/(1e-3*rgam)
      v_angle = 1./(1e-3*rgam)
      m.m_ang_h->Set_Value, h_angle, /floatvalue
      m.m_ang_v->Set_Value, v_angle, /floatvalue
      if Widget_info(event.top, /valid_id) then begin
         Widget_control, event.top, set_uvalue = m, /no_copy
      endif
      return
   endcase

endcase

if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = m, /no_copy
endif

end



Pro Make, Group = group

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

if (XRegistered( 'Make') ne 0) then return
m_top = Widget_base(title = "UNDULATOR POWER SPECTRUM", /row, $
                    xoffset=20, yoffset = 20)
m_base = Widget_base(m_top, /column, /sensitive)
m_base1 = Widget_base(m_base, /row)
m_help = Widget_button(m_base1, frame = 0, value = 'HELP ', uvalue = "M_HELP")
m_exit = Widget_button(m_base1, frame = 0, value = 'EXIT ', uvalue = "M_EXIT")
space = Widget_label(m_base, frame = 0, value ='')
space = Widget_label(m_base, frame = 0, value ='')
label = Widget_label(m_base, frame = 0, /align_left, $
         value ='NOTE: Default parameters loaded are for APS undulator A at closed gap')
space = Widget_label(m_base, frame = 0, value ='')
label = Widget_label(m_base, frame = 0, /align_left, value ='Input parameters:')
space = Widget_label(m_base, frame = 0, value ='')
choice_base = Widget_base(m_base, frame = 1, /column)
k_choice = CW_bgroup(choice_base, /row, /exclusive, /no_release, frame = 0, $
                   ['Enter deflection parameter k:           OR               ', $
                    'Enter first harmonic energy (KeV):'], $
                    uvalue ="K_OR_E", set_value = 0)
c_1=Widget_base(choice_base, /row)
c_1a=Widget_base(c_1, /column, /sensitive)
m_k =FSC_Inputfield(c_1a, title ='', /floatvalue, decimal = 3, value = 2.76, $
                    uvalue = "M_K", event_pro = 'Make_event', /focus_events)
space = Widget_label(c_1, frame = 0, value ='                                  ')
c_1b=Widget_base(c_1, /column, sensitive = 0)
m_efirst=FSC_Inputfield(c_1b, title ='', /floatvalue, value = 0, uvalue = "M_EFIRST", $
                        event_pro = 'Make_event', /focus_events)

space = Widget_label(m_base, frame = 0, value ='')
m_nharms = FSC_Inputfield(m_base,  /align_left, value = '', uvalue = "N_HARMS", $
     title ='Number of harmonics to calculate (if blank, all harmonics will be calculated):')
m_lamb = FSC_Inputfield(m_base, title ='Undulator period length (cm):', /floatvalue, $
                        decimal = 2, value = 3.3)
m_rgam = FSC_Inputfield(m_base, title ='Relativistic gamma:', /floatvalue, value = 13700, $
                        uvalue = "M_RGAM", event_pro = 'Make_event', /focus_events)
m_nper = FSC_Inputfield(m_base, title ='Number of periods:', /integervalue, value = 72)
space = Widget_label(m_base, frame = 0, value ='')
m_base3=Widget_base(m_base, /row)
m_base3a=Widget_base(m_base3, /column)
label = Widget_label(m_base3a, /align_left,  frame = 0, value ='Beam size (mm):')
m_sigx = FSC_Inputfield(m_base3a, title ='sigx:', /floatvalue, decimal = 5, value = 0.352)
m_sigy = FSC_Inputfield(m_base3a, title ='sigy:', /floatvalue, decimal = 5, value = 0.018)
space = Widget_label(m_base3, frame = 0, value ='          ')
m_base3b=Widget_base(m_base3, /column)
label = Widget_label(m_base3b, /align_left,  frame=0, value ='Beam divergence (mrad):')
m_sigx_prim = FSC_Inputfield(m_base3b, title ='sigx`:', /floatvalue, decimal = 5, value = 0.022)
m_sigy_prim = FSC_Inputfield(m_base3b, title ='sigy`:', /floatvalue, decimal = 5, value = 0.0042)
space = Widget_label(m_base, frame = 0, value ='')
gscl_chc_base = Widget_base(m_base, frame = 1, /column)
gscl_chc = CW_bgroup(gscl_chc_base, /row, /exclusive, /no_release, frame = 0, $
                   ['Angular range (quadrant in mrad):              OR            ', $
                    'Enter a number in unit of 1/gamma:'], $
                    uvalue ="A_OR_NUM", set_value = 0)
choice_1=Widget_base(gscl_chc_base, /row)
choice_1a=Widget_base(choice_1, /column, /sensitive)
m_ang_h = FSC_Inputfield(choice_1a, title = 'horizontal:', /floatvalue, decimal = 4, $
                          value = (2.76/(1e-3*13700.)))
m_ang_v = FSC_Inputfield(choice_1a, title = 'vertical:', /floatvalue, decimal = 4, $
                          value = (1./(1e-3*13700.)))
space = Widget_label(choice_1, frame = 0, value ='                              ')
choice_1b = Widget_base(choice_1, /column, sensitive = 0)
gscl_1 = FSC_Inputfield(choice_1b, title = 'horizontal:', /floatvalue, decimal = 2, $
                          value = 0)
gscl_2 = FSC_Inputfield(choice_1b, title = 'vertical:    ', /floatvalue, decimal = 2, $
                          value = 0)
space = Widget_label(m_base, frame = 0, value ='')
label = Widget_label(m_base, frame = 0, /align_left, $
      value = 'Number of points (in one quadrant, determines the grid density):')
m_xpt = FSC_Inputfield(m_base, title = 'x points:', /integervalue, value = 80)
m_ypt = FSC_Inputfield(m_base, title = 'y points:', /integervalue, value = 40)
space = Widget_label(m_base, frame = 0, value ='')
k_name = string(m_k->get_value())
f_name = STRCOMPRESS( k_name , /REMOVE_ALL )
split = STRSPLIT(f_name, '.', /extract)
f_name = 'u_k'+strjoin(split, 'p')
while strlen(f_name) eq (STRPOS(f_name, '0', /REVERSE_SEARCH)+1) do begin
   f_name = STRMID(f_name, 0, (strlen(f_name) - 1))
endwhile
m_filename = FSC_Inputfield(m_base, title = 'Enter a filename in the form of u_kxpx:', $
             value = f_name)
space = Widget_label(m_base, frame = 0, value ='')
space = Widget_label(m_base, frame = 0, value ='')
m_calc=Widget_button(m_base, /align_center, value ='  GENERATE SPECTRUM  ', uvalue ="M_CALC")
space = Widget_label(m_base, frame = 0, value ='')
space = Widget_label(m_top, frame = 0, value ='        ')
outbase = Widget_base(m_top, /column)
display = Widget_text(outbase, frame = 0, value = '', xsize = 60, ysize = 54)
space = Widget_label(m_top, frame = 0, value = ' ')

m_k->SetTabNext, m_nharms->GetTextID()
m_nharms->SetTabNext, m_lamb->GetTextID()
m_lamb->SetTabNext, m_rgam->GetTextID()
m_rgam->SetTabNext, m_nper->GetTextID()
m_nper->SetTabNext, m_sigx->GetTextID()
m_sigx->SetTabNext, m_sigy->GetTextID()
m_sigy->SetTabNext, m_sigx_prim->GetTextID()
m_sigx_prim->SetTabNext, m_sigy_prim->GetTextID()
m_sigy_prim->SetTabNext, m_ang_h->GetTextID()
m_ang_h->SetTabNext, m_ang_v->GetTextID()
m_ang_v->SetTabNext, m_xpt->GetTextID()
m_xpt->SetTabNext, m_ypt->GetTextID()
m_ypt->SetTabNext, m_filename->GetTextID()
m_filename->SetTabNext, m_k->GetTextID()

Widget_control, m_top, /realize
m_info = {k_choice:k_choice, c_1a:c_1a, m_k:m_k, c_1b:c_1b, m_efirst:m_efirst, m_nharms:m_nharms, $
          m_lamb:m_lamb, m_rgam:m_rgam, m_nper:m_nper, m_sigx:m_sigx, m_sigy:m_sigy, $
          m_sigx_prim:m_sigx_prim, m_sigy_prim:m_sigy_prim, choice_1a:choice_1a, choice_1b:choice_1b, $
          gscl_chc:gscl_chc, m_ang_h:m_ang_h, m_ang_v:m_ang_v, gscl_1:gscl_1, gscl_2:gscl_2, $
          m_xpt:m_xpt, m_ypt:m_ypt, m_filename:m_filename, display:display, m_base:m_base}
Widget_control, m_top, set_uvalue = m_info, /no_copy

XManager, 'Make', m_top, group_leader = group, event_handler ='Make_event'

end





