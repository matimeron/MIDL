;***********Fit event handler***********
Pro Sruff_fit_event, event

Widget_control, event.top, get_uvalue = f, /no_copy
if N_Elements(f.f_xsize->Get_Value()) eq 0 then f.f_xsize->Set_Value, 0.
if N_Elements(f.f_ysize->Get_Value()) eq 0 then f.f_ysize->Set_Value, 0.
xsize_f = f.f_xsize->get_value()
ysize_f = f.f_ysize->get_value()
slice_num = f.f_slice_index

Widget_control, f.f_choice, get_value = choice_f
Wset, f.f_winIndex
if choice_f eq 0 then choice = [0,0,0]
if choice_f eq 1 then choice = [1,0,0]
if choice_f eq 2 then choice = [1,1,1]


winSize = [xsize_f, ysize_f]
pow_f = f.pow
slice_mode = f.mode

Widget_control, event.id, get_uvalue = eventval
case eventval of

  "FCHOICE1": begin
     if Widget_info(event.top, /valid_id) then begin
        Widget_control, event.top, set_uvalue = f, /no_copy
     endif
  return
  endcase

  "FHELP1": begin
      XDisplayFile, "", $
        title = "Help for gaussian fit calculation", $
        group = event.top, $
        width = 70, $
        height = 20, $
        done_button = "Exit", $
        text = ['This routine fits the output data matrix into a Gaussian formula',$
        '', '', 'Description of input parameters for the formula fit calculation:', $
        '', '', 'x: range of fit in x, input unit = mm,  quadrant',$
        '       example:  x (mm): 4','', $
        'y: range of fit in y, input unit = mm, quadrant',$
        '       example:  y (mm): 2','', $
        'standard gaussian:  second order gaussian with x^2,  y^2 terms','', $
        'high order gaussian in x:  gaussian with x^4,  x^2,  y^2 terms','', $
        'high order gaussian in all terms:  gaussian with x^4,  y^4,  x^2,  y^2,  x^2y^2 terms']

  endcase

  "FEXIT1": Widget_control, event.top, /destroy

  "FFIT1": begin
     ;***********Begin input error checking***********
     if xsize_f le 0. then begin
        message_out = DIALOG_MESSAGE( ['Fitting range x parameter cannot be negative or zero!','',$
                                  'Please enter a value for the x fitting range.', $
                                  '(Note: quadrant only and units are mm)'], $
                                   DIALOG_PARENT = event.top, /ERROR, title = 'WARNING!')
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = f, /no_copy
        endif
     return
     endif
     if ysize_f le 0. then begin
        message_out = DIALOG_MESSAGE( ['Fitting range y parameter cannot be negative or zero!','',$
                                  'Please enter a value for the y fitting range.', $
                                  '(Note: quadrant only and units are mm)'], $
                                   DIALOG_PARENT = event.top, /ERROR, title = 'WARNING!')
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = f, /no_copy
        endif
     return
     endif
     if (xsize_f gt f.f_winx) or (ysize_f gt f.f_winy) then begin
        if (xsize_f gt f.f_winx) then begin
           xsize_f = f.f_winx
           message_out = DIALOG_MESSAGE( $
        ['Fitting range is quadrant only and must be less than or equal to 1/2 full aperture size', $
         '', $
         'The x size will automatically be reset to largest value allowed for the aperture.'],$
          DIALOG_PARENT = event.top, /ERROR, title = 'WARNING!')
           f.f_xsize->Set_Value, xsize_f, /floatvalue
        endif
        if (ysize_f gt f.f_winy) then begin
           ysize_f = f.f_winy
           message_out = DIALOG_MESSAGE($
        ['Fitting range is quadrant only and must be less than or equal to 1/2 full aperture size', $
         '', $
         'The y size will automatically be reset to largest value allowed for the full aperture.'],$
             DIALOG_PARENT = event.top, /ERROR, title = 'WARNING!')
           f.f_ysize->Set_Value, ysize_f, /floatvalue
        endif
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = f, /no_copy
        endif
        return
     endif

     ;***********End input error checking***********
     if slice_mode eq 0 then begin
        result = xyfit(pow_f, /show_fit, /gauss, integrals=in, /weight, $
              /peak, win=winSize, quartic = choice)
     endif else begin
        result = xyfit(pow_f, /show_fit, /gauss, integrals=in, /weight, $
        /peak, win=winSize, slice = slice_num, quartic = choice)
     endelse
     Widget_control, f.f_result1, set_value = in(0)
     Widget_control, f.f_result2, set_value = in(1)
     Widget_control, f.f_base1a1, sensitive = 0
     Widget_control, f.f_calc_base, sensitive = 0
     Widget_control, f.f_output, /sensitive
     Widget_control, f.recalc, /sensitive

  endcase

  "FNEWFIT1": begin
     Widget_control, f.f_base1a1, /sensitive
     Widget_control, f.f_calc_base, /sensitive
     Widget_control, f.f_output, sensitive = 0
     Widget_control, f.recalc, sensitive = 0
  endcase

  "FPRINT1": begin
     printer_chc = dialog_printersetup()
     if printer_chc eq 0 then begin
        if Widget_info(event.top, /valid_id) then begin
           Widget_control, event.top, set_uvalue = f, /no_copy
        endif
        return
     endif
     if slice_mode eq 0 then begin
         output, $
         'result = xyfit(pow_f, /show_fit, /gau, /weight, /peak, win=winSize, qua = choice)',$
        sub ='pow_f, winSize, choice', pow_f, winSize, choice, DEVICE = 'PRINTER'
     endif else begin
        output, $
        'result = xyfit(pow_f, /show_fit, /gau, /weight, /peak, win=winSize, slice = slice_num, qua = choice)',$
         sub ='pow_f, winSize, slice_num, choice', pow_f, winSize, slice_num, choice, DEVICE = 'PRINTER'
    endelse
  endcase

   "FSAVE1": begin
      f_outfile = dialog_pickfile(dialog_parent = event.top, get_path=path_f, $
      path = 'C:\my documents',title = 'Please Select a Folder and Enter a Filename', /write)
      if f_outfile eq '' then begin
         if Widget_info(event.top, /valid_id) then begin
            Widget_control, event.top, set_uvalue = f, /no_copy
         endif
         return
      endif

      if slice_mode eq 0 then begin
         output, $
         'result = xyfit(pow_f, /show_fit, /gau, /weight, /peak, win=winSize, qua = choice)',$
        sub ='pow_f, winSize, choice', pow_f, winSize, choice, DEVICE = 'EPS', FIL = f_outfile
      endif else begin
        output, $
        'result = xyfit(pow_f, /show_fit, /gau, /weight, /peak, win=winSize, slice = slice_num, qua = choice)',$
         sub ='pow_f, winSize, slice_num, choice', pow_f, winSize, slice_num, choice, $
         DEVICE = 'EPS', FIL = f_outfile
     endelse
   endcase

   "SLICE_CHOICE": begin
      f.f_slice_index = event.index
   endcase

endcase

if Widget_info(event.top, /valid_id) then begin
        Widget_control, event.top, set_uvalue = f, /no_copy
endif
end


;***********Power fitting input display widgets***********
Pro Sruff_fit, f_opow, f_winx, f_winy, y_n_slice, depths, Group = group

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

pow = f_opow

if (XRegistered( 'Sruff_fit') ne 0) then return
if y_n_slice eq 1 then mode = 1 else mode = 0
f_base=Widget_base(title="FORMULA FIT", /sensitive, $
                   xoffset = 20, yoffset = 20)
f_base1=Widget_base(f_base, /row)
f_base1a=Widget_base(f_base1, /column)
f_base1b=Widget_base(f_base1, /column)
f_base2a=Widget_base(f_base1a, /row)
f_help=Widget_button(f_base2a, frame = 0, value = '  HELP  ', uvalue = "FHELP1")
f_exit=Widget_button(f_base2a, frame = 0, value = '  EXIT  ', uvalue = "FEXIT1")
space=Widget_label(f_base1a, frame = 0, value ='')
space=Widget_label(f_base1a, frame = 0, value ='')
f_base1a1=Widget_base(f_base1a, /column, /sensitive)
slice_base = Widget_base(f_base1a1, /row, /align_left, map = mode)
f_slice = Widget_droplist(slice_base, /dynamic_resize, frame = 0, value = depths, $
                         title ='Select slice to fit:', uvalue = "SLICE_CHOICE")
space=Widget_label(f_base1a1, frame = 0, value ='')
f_label=Widget_label(f_base1a1, frame = 0, /align_left, value ='Fitting range (quadrant only):')
f_xsize=FSC_Inputfield(f_base1a1, frame = 0, title ='x (mm):', /floatvalue, decimal = 2, $
        value = f_winx)
f_ysize=FSC_Inputfield(f_base1a1, frame = 0, title ='y (mm):', /floatvalue, decimal = 2, $
        value = f_winy)
space=Widget_label(f_base1a1, frame = 0, value ='')
space=Widget_label(f_base1a1, frame = 0, value ='')
f_label=Widget_label(f_base1a1, frame = 0, /align_left, value ='Terms of fitting:')
f_choice=CW_bgroup(f_base1a1, /column, /exclusive, /no_release, $
                   ['standard gaussian (x^2, y^2)', 'high order gaussian in x (x^4, x^2, y^2)', $
                    'high order gaussian in all terms (x^4, y^4, x^2 y^2, x^2, y^2, x^2y^2)'], $
                    uvalue ="FCHOICE1",set_value = 0)
space=Widget_label(f_base1a1, frame = 0, value ='')
space=Widget_label(f_base1a1, frame = 0, value ='')
f_calc_base=Widget_base(f_base1a, frame = 0, /column, /align_center, /sensitive)
f_fit=Widget_button(f_calc_base, frame = 0, /align_center, value =' PERFORM FIT ', uvalue = "FFIT1")
space=Widget_label(f_base1a, frame = 0, value ='')
space=Widget_label(f_base1a, frame = 0, value ='')
f_output =Widget_base(f_base1a, frame = 1, /align_center, sensitive = 0, /column)
label=Widget_label(f_output, /align_center ,  frame = 0, value ='Output options:')
space = Widget_Label(f_output, frame = 0, value ='')
output1=Widget_base(f_output,/align_center, frame = 0, /row)
f_print=Widget_button(output1, frame = 0, /align_center, value ='      PRINT      ', uvalue ="FPRINT1")
f_save=Widget_button(output1, frame = 0, /align_center, value =' SAVE AS EPS FILE ', $
                      uvalue ="FSAVE1")
space = Widget_label(f_base1a, frame = 0, value ='')
space = Widget_label(f_base1a, frame=0, value ='')
recalc=Widget_base(f_base1a, frame = 0, sensitive = 0, /align_center, /row)
f_newcalc=Widget_button(recalc, /align_center, frame = 0, value =' NEW FIT CALCULATION ', $
                        uvalue = "FNEWFIT1")
f_plot=Widget_draw(f_base1b, /frame, xsize = 768, ysize = 512)
space=Widget_label(f_base1b, frame = 0, value ='')
f_base2b=Widget_base(f_base1b, /row, /align_center)
f_result1=CW_field(f_base2b, title ='Integrated raw power (w):', /floating, value = 0, /noedit)
space=Widget_label(f_base2b, frame = 0, value ='        ')
f_result2=CW_field(f_base2b, title ='Integrated fitted power (w):', /floating, value = 0, /noedit)

f_xsize->SetTabNext, f_ysize->GetTextID()
f_ysize->SetTabNext, f_xsize->GetTextID()

f_slice_index = 0
Widget_control, f_base, /realize
Widget_control, f_plot, get_value = f_winIndex
f_info = {f_base1a1:f_base1a1, f_xsize:f_xsize, f_ysize:f_ysize, f_choice:f_choice, $
          f_calc_base:f_calc_base, f_output:f_output, recalc:recalc, f_result1:f_result1, $
           f_result2:f_result2, f_winIndex:f_winIndex, pow:pow, f_winx:f_winx, $
           f_winy:f_winy, mode:mode, f_slice_index:f_slice_index}
Widget_control, f_base, set_uvalue = f_info, /no_copy
XManager, 'Sruff_fit', f_base, group_leader = group, event_handler ='Sruff_fit_event'

end