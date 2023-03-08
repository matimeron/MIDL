Pro X19C_comm, initialize = ini, degrees = deg, $
    x_mono = xmo, theta_mono = tetmo, chi = chi, phi = phi, x_crystal = xcrys, $
    two_theta = totet, theta_samp = tetsam, y_samp = ysam, $
    theta_det = tetdet, y_det = ydet, $
    inclination = einc, two_d = etodee, r_samp = ersam, r_det = erdet, $
    totet_offset = etotoff, beam = ebeam

;+
; NAME:
;	X19C_COMM
; PURPOSE:
;	Models the X19C Monochromator - Spectrometer system.
; CATEGORY:
;	X-ray optics
; CALLING SEQUENCE:
;	X19C_COMM [, keywords]
; INPUTS:
;	None
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    INITIALIZE
;	Switch.  If set, the common blocks and the device structures are 
;	initialized and all the variables set to their default values unless
;	overridden by other keywords.  An implicit INITIALIZE is performed when
;	X19C_COMM is called the first time.
;    DEGREES
;	Switch.  Specifies that all the angle values (with the exception of 
;	INCLINATION (see below) are specified in degrees.  Default is radians.
;    X_MONO
;	The X position of the monochromator.  Init. default is 0.
;    THETA_MONO
;	The Theta angle of the monochromator.  Init. default is 0.
;    CHI
;	The Chi angle of the monochromator.  Init. default is 0.
;    PHI
;	The Phi angle of the monochromator.  Init. default is 0.
;    X_CRYSTAL
;	The X position of the monochromator crystal.  Init. default is 0.
;    TWO_THETA
;	The 2-Theta angle of the spectrometer.  Init. default is 0.
;    THETA_SAMP
;	The Theta angle of the target.  Init. default is 0.
;    Y_SAMP
;	The Y position of the target.  Init. default is 0.
;    THETA_DET
;	The Theta angle of the detector.  Init. default is 0.
;    Y_DET
;	The Y position of the detector.  Init. default is 0.
;    INCLINATION
;	The angle between the beam and the horizontal direction.  Always given
;	in radians.  Init. default is 0.009.
;    TWO_D
;	The 2d value for the reflecting crystal, Init. default is 6.26 (Si 111)
;    R_SAMP
;	The radius of the 2-theta circle.  Init. default is 1000.
;    R_SAMP
;	The radius of the detector theta circle.  Init. default is 1000.
;    TOTET_OFFSET
;	The offset of the 2-theta axis from the origin.  Provided as a 3 
;	dimensional vector.  Init. default is [0,0,0]
;    BEAM
;	The X-ray beam, provided in the form of a GELEM structure representing 
;	a line (see routine MAKE_ELEM in XOPER_LIB).  Init. default is: 
;	location (0,0,0), direction (0,0,1).
; OUTPUTS:
;	None.  All the communication is done through common blocks.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;    Common block X19C.  The listing below includes names of variables, 
;    with explanations and names of relevant keywords (when applicable).
;
;	READY - Internal parameter, defined during initialization.  Its only
;		purpose is to indicate that initialization has been performed.
;	ORI - 	Origin vector, (0, 0, 0).
;	X - 	X vector, (1, 0, 0).
;	Y - 	Y vector, (0, 1, 0).
;	Z - 	Z vector, (0, 0, 1).
;	INC -	The angle between the beam and the horizontal direction. 
;		Set by the keyword INCLINATION.
;	TODEE - Value of crystal's 2d.  Set by the keyword TWO_D.
;	RSAM -	Value of the 2-Theta radius.  Set by keyword R_SAMP.
;	RDET -	Value of the detector Theta radius.  Set by keyword R_DET.
;	TOTOFF-	Values of 2-Theta's axis offset (vector).  
;		Set by keyword TOTET_OFFSET.
;	BEAM -	The X-ray beam, represented by a line type GELEM structure.
;		Either created automatically or set by the BEAM keyword.
;	NHU -	Number of stages in the MONOC (monochrometer) device minus 1.
;	NSP -	Number of stages in the SPECT (spectrometer) device minus 1.
;	MONOC -	An array of GELEM structures representing the monochrometer.
;	SPECT -	An array of GELEM structures representing the spectrometer.
;	MONOC_PARS - Floating array, contains the values set by (in order) 
;		X_MONO, THETA_MONO, CHI, PHI, X_CRYSTAL.
;	SPECT_PARS = Floating array, contains the values set by (in order)
;		TWO_THETA, THETA_SAMP, Y_SAMP, THETA_DET, Y_DET.
; SIDE EFFECTS:
;	Some or all of the variables in the common blocks may change when
;	X19C_COMM is called.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	On init defines the structures that model the monochrometor and the 
;	spectrometer.  Then rotations and translations are used to reposition 
;	the individual elements.  On any subsequent call rotations and 
;	translations are performed to reflect the changes in the values of the 
;	parameters since the previous call.
;	Uses calls to the following library routines:
;	From MIDL:  DEFAULT, TYPE, INT.
;	From XOPER:  MAKE_LINE, MAKE_PLANE, ROTATE_ELEM, MOVE_DEVICE.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 15-JULY-1992 by Mati Meron.
;-

    common x19c, ready, ori, x, y, z, inc, todee, rsam, rdet, totoff, beam, $
    nmo, nsp, monoc, spect, monoc_pars, spect_pars

    on_error, 1

    if n_elements(ready) eq 0 then begin
	ori = [0., 0., 0.]
	x = [1., 0., 0.]
	y = [0. ,1., 0.]
	z = [0., 0., 1.]

	nmo = 4
	nsp = 4
	ilin = Make_line(ori,x)
	monoc = replicate(ilin,nmo + 1)
	spect = replicate(ilin,nsp + 1)
	ready = 1
	ini = 1
    endif

    if keyword_set(ini) then begin
	inc = 1.*Default(einc, Default(inc, 9e-3))
	todee = 1.*Default(etodee, Default(todee, 6.26))
	rsam = 1.*Default(ersam, Default(rsam,1000))
	rdet = 1.*Default(erdet, Default(rdet,1000))
	totoff = 1.*Default(etotoff, ori)
	ilin = Default(ilin, Make_line(ori,x))
	beam = Rotate_elem(Default(ebeam, Make_line(ori,z,name='Beam'),typ=8),$
		ilin, inc)

	monoc(0) = Make_plane(ori, x, name = 'Mono X stage')
	monoc(1) = Make_line(ori, y, name = 'Mono Theta stage')
	monoc(2) = Make_line(ori, z, name = 'Mono Chi stage')
	monoc(3) = Make_line(ori, y, name = 'Mono Phi stage')
	monoc(4) = Make_plane(ori, x, name = 'Crystal X stage')
	Move_device, monoc, inc, external = ilin

	spect(0) = Make_line(ori + totoff, y, name = '2_Theta stage')
	spect(1) = Make_line(spect(0).loc + rsam*z, y, $
		name = 'Sample Theta stage', idnum = 2)
	spect(2) = Make_plane(spect(1).loc, y, $
		name = 'Sample Y stage', idnum = 2)
	spect(3) = Make_line(spect(1).loc, y, $
		name = 'Det Theta stage', idnum  = 3)
	spect(4) = Make_plane(spect(3).loc + rdet*z, y, $
		name = 'Det Y stage', idnum = 3)

	monoc_pars = fltarr(nmo + 1)
	spect_pars = fltarr(nsp + 1)
    endif else begin
	vnams = ['Inclination','2d','R_samp','R_det','2theta offset','Beam']
	dum = One_of(einc,etodee,ersam,erdet,etotoff,ebeam)
	if dum ge 0 then message, $
	'The INI option has to be set to change ' + vnams(dum)
    endelse

    if keyword_set(deg) then dmult = !dtor else dmult = 1.
    topi = 2*!pi

    monoc_npars = monoc_pars
    dum = monoc.dim
    dum(where(dum ne 1)) = -1
    if n_elements(xmo) eq 0 then dum(0) = -1 else monoc_npars(0) = xmo
    if n_elements(tetmo) eq 0 then dum(1) = -1 else monoc_npars(1) = tetmo
    if n_elements(chi) eq 0 then dum(2) = -1 else monoc_npars(2) = chi
    if n_elements(phi) eq 0 then dum(3) = -1 else monoc_npars(3) = phi
    if n_elements(xcrys) eq 0 then dum(4) = -1 else monoc_npars(4) = xcrys
    dum = where(dum ge 0, ndum)
    if ndum gt 0 then begin
	angs = dmult*monoc_npars(dum)
	monoc_npars(dum) = angs + topi*Int(0.5 - angs/topi)
    endif

    spect_npars = spect_pars
    dum = spect.dim
    dum(where(dum ne 1)) = -1
    if n_elements(totet) eq 0 then dum(0) = -1 else spect_npars(0) = totet
    if n_elements(tetsam) eq 0 then dum(1) = -1 else spect_npars(1) = tetsam
    if n_elements(ysam) eq 0 then dum(2) = -1 else spect_npars(2) = ysam
    if n_elements(tetdet) eq 0 then dum(3) = -1 else spect_npars(3) = tetdet
    if n_elements(ydet) eq 0 then dum(4) = -1 else spect_npars(4) = ydet
    dum = where(dum ge 0, ndum)
    if ndum gt 0 then begin
	angs = dmult*spect_npars(dum)
	spect_npars(dum) = angs + topi*Int(0.5 - angs/topi)
    endif

    Move_device, monoc, monoc_npars - monoc_pars
    monoc_pars = monoc_npars

    Move_device, spect, spect_npars - spect_pars
    spect_pars = spect_npars

    return
end
