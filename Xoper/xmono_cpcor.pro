Pro Xmono_cpcor, chiran, phiran, enpoints, title = tit, degrees = edeg, $
    theta = etet, x_crystal = excrys, totet_offset = etotoff, beam = ebeam

;+
; NAME:
;	XMONO_CPCOR
; PURPOSE:
;	Generates a Chi-Phi map of the deviations in reflected beam position
;	caused by errors in monochrometer, spectrometer or beam positioning.
; CATEGORY:
;	X-ray optics
; CALLING SEQUENCE:
;	XMONO_CPCOR [, CHIRAN, PHIRAN, ENPOINTS ][, keywords]
; INPUTS:
;    CHIRAN
;	Range of Chi values, provided as a 2-element vector.
;    PHIRAN
;	Range of Phi values, provided as a 2-element vector.
;    ENPOINTS
;	Number of points to use in each dimension.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    TITLE
;	Accepts a character string to be used as the title of the plot.
;	Default is "Beam offsets on target".
;    DEGREES
;	Switch.  Specifies that all the angle values (with the exception of 
;	INCLINATION (see below) are specified in degrees.  Default is radians.
;    THETA
;	The Theta angle of the monochromator.  Default is 0.
;    X_CRYSTAL
;	X position of the monochromator crystal.  Default is 0.
;    TOTET_OFFSET
;	The offset of the 2-theta axis from the origin.  Provided as a 3 
;	dimensional vector.  Default is [0,0,0]
;    BEAM
;	The X-ray beam, provided in the form of a GELEM structure representing 
;	a line (see routine MAKE_ELEM in XOPER_LIB).  Default is: 
;	location (0,0,0), direction (0,0,1).
; OUTPUTS:
;	None other then output to the current display device.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	One block, STANDARD.  Contains the following:
;
;    Common block BASIC:
;	NPOINTS -	Value of ENPOINTS.
;	DEG -		Latest setting of the DEGREES switch.
;	CPGRID -	The Chi-Phi grid used in calculations.
;	STOT -	Standard Two_theta values (no errors).
;	SYOT -	Standard sample's Y values (no errors).
; SIDE EFFECTS:
;	Changes values in the common blocks maintained in XMONO_COMM.
; RESTRICTIONS:
;	At least one of the keyword parameters THETA, X_CRYSTAL, TOTET_OFFSET
;	or BEAM should be used.
; PROCEDURE:
;	If ENPOINTS is given creates a new grid of Chi-Phi values and 
;	calculates standard 2-Thata and Y values over this grid, otherwise 
;	reuses previously established grid and standard values.  Calculates the
;	offset values and displays the difference between these and the 
;	standard values in the form of a 2-dim contour plot.
;	From MIDL:  DEFAULT, MAKE_GRID, ONE_OF, PLVAR_KEEP.
;	From XOPER:  XMONO_COMM, XSAM_INTER.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    common standard, npoints, deg, cpgrid, stot, syot

    on_error, 1
    Xmono_comm, /ini

    if n_elements(enpoints) ne 0 or n_elements(cpgrid) eq 0 then begin
	deg = Default(edeg,0)
	nran = abs(chiran(1) - chiran(0)) > abs(phiran(1) - phiran(0))
	if deg eq 0 then nran = Nint(!radeg*nran)
	npoints = Default(enpoints, nran > 10)
	cpgrid = Make_grid([[chiran],[phiran]], npoints + 1, fun = stot)
	syot = stot

	for i = 0, npoints do begin
	    for j = 0, npoints do begin
		Xmono_comm, chi= -cpgrid(0,i,j), phi= cpgrid(1,i,j), degr = deg
		stot(i,j) = Xsam_inter(y_sam = dum, degrees = deg)
		syot(i,j) = dum
	    endfor
	endfor
    endif

    if One_of(etet,excrys,etotoff,ebeam) eq -1 then $
    message, 'Some offset should be provided!'

    Xmono_comm, /ini, degrees = deg, theta = etet, x_crystal = excrys, $
    totet_offset = etotoff, beam = ebeam
    tot = stot
    yot = syot
    for i = 0, npoints do begin
	for j = 0, npoints do begin
	    Xmono_comm, chi = -cpgrid(0,i,j), phi = cpgrid(1,i,j) - etet, $
	    degr = deg
	    tot(i,j) = Xsam_inter(y_sam = dum, degrees = deg)
	    yot(i,j) = dum
	endfor
    endfor
    tot = tot - stot
    yot = yot - syot

    Plvar_keep, act = 'save'

    !p.multi = [0,2,1]
    !x.style = 1
    !y.style = 1
    !x.thick = 2
    !y.thick = 2

    xtit = 'Chi'
    ytit = 'Phi'
    yform = '(f6.3)'
    if deg then begin
	tform = '(f6.3)'
	sdeg = ' (degrees)' 
    endif else begin
	tform = '(e9.2)'
	sdeg = ''
    endelse

    !p.position = [0.02, 0.1, 0.45, 0.7]
    !y.tickname = ' '
    maxl = max(tot, min = minl)
    clev = minl + ((maxl - minl)/20.)*(1. + 3.*findgen(7))
    cann = string(clev, form = tform)
    contour, tot, reform(cpgrid(0,*,0)), reform(cpgrid(1,0,*)), /follow, $
    levels = clev, c_annotation = cann, xtitle = xtit + sdeg, $
    title = 'Target 2-theta offset' + sdeg
    !y.tickname = ''
    axis, yaxis = 1

    !p.position = [0.55, 0.1, 0.98, 0.7]
    maxl = max(yot, min = minl)
    clev = minl + ((maxl - minl)/20.)*(1. + 3.*findgen(7))
    cann = string(clev, form = tform)
    contour, yot, reform(cpgrid(0,*,0)), reform(cpgrid(1,0,*)), /follow, $
    levels = clev, c_annotation = cann, xtitle = xtit + sdeg, $
    ytitle = ytit + sdeg, title = 'Target  Y(vertical) offset (mm)'

    ropos = 0.95
    lef = 0.05
    if !d.name eq 'TEK' then rit = 0.40 else rit = 0.55
    step = 0.05
    tit = Default(tit, 'Beam offsets on sample')

    ro = ropos
    xyouts, 0.5, ro, tit, charthick = 2, align = 0.5, charsize = 1.5, /normal
    if n_elements(etet) ne 0 then begin
	ro = ro - step
	xyouts, lef, ro, 'Theta     = ' + string(etet,format = tform), /normal
    endif
    if n_elements(excrys) ne 0 then begin
	ro = ro - step
	xyouts, lef, ro, 'X-Crystal = ' + string(excrys, format=yform), /normal
    endif

    ro = ropos
    if n_elements(etotoff) ne 0 then begin
	ro = ro - step
	out = '2-theta axis offset = ['
	for i = 0, 2 do out = out + string(etotoff(i),format = tform)
	out = out + ']'
	xyouts, rit, ro, out, /normal
    endif
    if n_elements(ebeam) ne 0 then begin
	ro = ro - step
	out = 'Beam loc. offset = ['
	for i = 0, 2 do out = out + string(ebeam.loc(i) ,format = yform)
	out = out + ']'
	xyouts, rit, ro, out, /normal
	ro = ro - step
	dumd = ebeam.dir - [0,0,1]
	out = 'Beam dir. offset = ['
	for i = 0, 2 do out = out + string(dumd(i) ,format = '(e10.3)')
	out = out + ']'
	xyouts, rit, ro, out, /normal
    endif

    Plvar_keep, act = 'restore'

    return
end
