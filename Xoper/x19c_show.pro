Pro X19C_show, full = ful

;+
; NAME:
;	X19C_SHOW
; PURPOSE:
;	Displays the information contained in the common blocks BASIC and X19C,
;	which are created by X19C_COMM.
; CATEGORY:
;	X-ray optics
; CALLING SEQUENCE:
;	X19C_SHOW [, /FULL]
; INPUTS:
;	None
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    FULL
;	Switch.  If set, all the coordinates of the individual stages in HUBER
;	and SPECT are displayed, otherwise only the external angles and 
;	locations are shown.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	Common block X19C.  For more info see routine X19C_COMM.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Calls TYPE from MIDL and SHOW_ELEM from XOPER.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    common x19c, ready, ori, x, y, z, inc, todee, rsam, rdet, totoff, beam, $
    nmo, nsp, monoc, spect, monoc_pars, spect_pars

    on_error, 1
    if Type(ready) eq 0 then message, 'Common blocks not initialized!'

    monames = ['X_Mono', 'Theta', 'Chi', 'Phi', 'X_Crystal']
    mdegs = strarr(nmo + 1)
    dum = where(monoc.dim eq 1)
    mdegs(dum) = '('+string(!radeg*monoc_pars(dum),format='(f9.4)')+' degrees)'

    spnames = ['2_Theta', 'Theta_samp', 'Y_samp', 'Theta_det', 'Y_det']
    sdegs = strarr(nsp + 1)
    dum = where(spect.dim eq 1)
    sdegs(dum) = '('+string(!radeg*spect_pars(dum),format='(f9.4)')+' degrees)'

    print
    print, format ='("Inclination = ",f7.3," mrad",t40,"2d = ",f6.3," Ang.")',$
    inc, todee
    print, format ='("Samp. radius = ",f7.1,t40,"Det. radius = ",f7.1)', $
    rsam, rdet
    print
    for i = 0, nmo do print, format = '(a,t16,"= ",f11.5,tr4,a)', $
    monames(i), monoc_pars(i), mdegs(i)
    print
    for i = 0, nsp do print, format = '(a,t16,"= ",f11.5,tr4,a)', $
    spnames(i), spect_pars(i), sdegs(i)
    print

    print, 'Beam parameters:'
    Show_elem, beam

    if keyword_set(ful) then begin
	print
	hak, mesg = 'Hit any key to continue'
	clear_screen
	print, 'Monochromator:'
	print
	for i = 0, nmo do Show_elem, monoc(i), /nospace
	print
	hak, mesg = 'Hit any key to continue'
	clear_screen
	print, 'Spectrometer:'
	print
	for i = 0, nsp do Show_elem, spect(i), /nospace
    endif

    return
end
