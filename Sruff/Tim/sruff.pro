;***********Event handler for SRUFF main menu***********
Pro Sruff_event, event

common base_share, p_base, f_base, m_top, show_base, absorb_base, slice_base, d_base

Widget_control, event.id, get_uvalue = eventval

case eventval of

   "ABSORB" : begin
      if (XRegistered( 'Power') ne 0) then Widget_control, p_base, /destroy
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      Absorb, Group = event.top
   endcase

   "HELP": XDisplayFile, "", $
           title = "SRUFF HELP", $
           group = event.top, $
           width = 60, $
           height = 35, $
           done_button = "Exit", $
           text = ['SRUFF GRAPHICAL USER INTERFACE',$
                   '',$
                   '',$
                   'Drop down menu choices for options button:','','', $
                   'CALCULATE UNDULATOR RAW POWER DENSITY:',$
                   '   - This routine will calculate undulator raw total power', + $
                   '     and power density.','',$
                   'CALCULATE UNDULATOR SPECTRUM:', $
                   '   - This routine is used to generate a data structure of the ', $
                   '     undulator spectrum as a function of photon energy and location.', $
                   '   - An undulator spectrum file is required for the', $
                   '     CALCULATE ABSORBTION IN A MEDIUM', $
                   '     CALCULATE TRANSMISSION THROUGH A MEDIUM', $
                   '     and CALCULATE HARMONIC SIZE AND INTENSITY  routines.', $
                   '', 'CALCULATE ABSORBTION IN A MEDIUM:', $
                   '   - This routine calculates the absorbed power density in ', $
                   '      w/mm^3 in thin layers of filters and windows.','',  $
                   'CALCULATE TRANSMISSION THROUGH A MEDIUM:', $
                   '   - This routine calculates the transmitted or reflected ', + $
                   '      power from filters and mirrors.', '', $
                   'CALCULATE HARMONIC SIZE AND INTENSITY:', $
                   '   - This routine calculates the photon intensity for a ', $
                   '      specific energy slice.', $
                   '   - It also calculates the harmonic size.']

   "INFO": XDisplayFile, "", $
          title = "SRUFF INFORMATION", $
          group = event.top, $
          width = 60, $
          height = 15, $
          done_button = "Exit", $
          text = ['SRUFF PROGRAM PACKAGE',$
                  '',$
                  '',$
                  'SRUFF is a comprehensive X-ray source calculation package capable of ', $
                  'calculating power density distribution, power absorbtion, reflection, ', $
                  'photon intensity of specific energy and harmonic size.  Source code was ', $
                  'developed by Dr. Mati Meron, an APS resident user at CARS of the ', $
                  'University of Chicago.  The Graphic User Interface (GUI) was developed ', $
                  'by Mr. Timothy Stern as a student summer research project proposed and ', $
                  'advised by Ms. Yifei Jaski of APS XFD division XFE group at Argonne ', $
                  'National Laboratory.']


   "MAKE" : begin
      if (XRegistered( 'Power') ne 0) then Widget_control, p_base, /destroy
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      check_database = DIALOG_MESSAGE(['The undulator spectrum data structure is very large.',$
          'Computation time may be 40 min. on a typical PC with a 1GHz processor.',$
          'A database of previosly generated spectrum files exists.', $
          'You may view a list of parameters used to generate each spectrum by',$
          'selecting a file from the database.','','', $
          'Would you like to view the database?'],$
          DIALOG_PARENT = event.top, /question, title = 'CHECK DATABASE?')
      if check_database eq 'Yes' then begin
         sruff_database, Group = event.top
      endif
      if check_database eq 'No' then begin
         Make, Group = event.top
      endif
   endcase

   "POWER1": begin
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      Power, Group = event.top
   endcase

   "QUIT": Widget_control, event.top, /destroy

   "SHOW" : begin
      if (XRegistered( 'Power') ne 0) then Widget_control, p_base, /destroy
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      Show, Group = event.top
   endcase

   "SLICE" : begin
      if (XRegistered( 'Power') ne 0) then Widget_control, p_base, /destroy
      if (XRegistered( 'Sruff_fit') ne 0) then Widget_control, f_base, /destroy
      Slice, Group = event.top
   endcase

endcase
end


;***********SRUFF main menu display widgets***********
Pro Sruff

if (XRegistered( 'Sruff') ne 0) then return
s_base = Widget_base(title="SRUFF", /column, xoffset = 20, yoffset = 20)
s_base_1 = Widget_base(s_base, /row)
option_base = Widget_button(s_base_1, /align_left, value =' OPTIONS ', menu = 1)
s_power1 = Widget_button(option_base, value =' Undulator raw total power and density calculation ',$
                        uvalue = "POWER1",  /align_left)
s_make = Widget_button(option_base, /align_left, uvalue ="MAKE", $
                       value =' Generate undulator spectrum ')
s_absorb = Widget_button(option_base, /align_left, uvalue ="ABSORB", $
                         value =' Calculate absorbed power in thin media ')
s_show = Widget_button(option_base, /align_left, uvalue ="SHOW", $
                       value =' Calculate transmitted or reflected power through filters or mirrors ')
s_slice = Widget_button(option_base, /align_left, uvalue ="SLICE", $
                        value =' Calculate harmonic size and intensity ')
s_info= Widget_button(s_base_1, /align_left, value =' INFO ', uvalue ="INFO")
s_hlp= Widget_button(s_base_1, /align_left, value =' HELP ', uvalue ="HELP")
s_quit= Widget_button(s_base_1, /align_left, value =' QUIT ', uvalue ="QUIT")
s_logo=Widget_draw(s_base, /frame, xsize = 256, ysize = 256)
Widget_control, s_base, /realize
Widget_control, s_logo, get_value = s_winIndex
WSet,s_winIndex
;i = read_image('image1.bmp')
;tv, i, /true
XManager, 'Sruff', s_base, event_handler ='Sruff_event'

end