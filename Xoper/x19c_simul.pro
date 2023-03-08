Function X19C_simul, wavl, alpha, dkt, dkn, degrees = deg, kevun = kev, $
    left = lef, extreme = ext, status = st

;+
; NAME:
;	X19C_SIMUL
; PURPOSE:
;	Simulates the behavior of the X19C monochromator-spectrometer complex.
; CATEGORY:
;	X-ray optics.
; CALLING SEQUENCE:
;	Result = X19C_SIMUL (WAVL, ALPHA, DKT, DKN [, keywords ])
; INPUTS:
;    WAVL
;	X-ray wavelength in Angstrem (or energy in keV if KEVUN is set).
;    SURF
;	Downward beam inclination angle.
;    DKT
;	Relative total momentum change (delta(k)/k).  Used as input to 
;	SURFSCAT, see there for more detail.
;    DKN
;	Relative normal momentum change ((delta(k).normal)/k).  Used as input 
;	to SURFSCAT, see there for more detail.  Not needed if the keyword
;	EXTREME is used.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    DEGREES
;	Switch.  If set, the value of ALPHA is degrees.  Default is radians.
;    KEVUN
;	Switch.  If set, WAVL is assumed to be the X-ray energy in keV.
;    LEFT
;	Switch.  Used as an instruction to SURFSCAT, see there for more detail.
;    EXTREME
;	Accepts one of the character values 'UP', 'SIDE' or 'DOWN', and directs 
;	the momentum change appropriately.  Used as instruction to SURFSCAT.
;    STATUS
;	Optional output parameter, see below.
; OUTPUTS:
;	Returns a 3-element vector containing (in order) the deviations of the
;	beam energy, total scattering monmentum change and normal scattering 
;	momentum change from their assigned values.
; OPTIONAL OUTPUT PARAMETERS:
;    STATUS
;	The name of a variable to accept the status code of the operation,
;	1 for success, 0 for failure.
; COMMON BLOCKS:
;	common block X19C.  See routine X19C_comm for explanations.
; SIDE EFFECTS:
;	May change values of variables in the common block X19C.
; RESTRICTIONS:
;	Various geometrical restrictions apply.  WAVL must be within the range
;	that can be Bragg reflected by the crystal.  ALPHA must be within the 
;	possible inclination range (which depends on WAVL).  DKT should be less
;	than 2, and DKN (if used) should be less than DKT (how much less, 
;	depends on the other parameters.
; PROCEDURE:
;	Two step procedure.  First sets the monochromator to its zero (no 
;	offsets) position and finds the appropriate settings for the various
;	stages, which should yield the assigned energy, and momentum offset.
;	Then, using the calculated settings plus whatever offsets were assigned
;	previously, an beam trajectory through the setup is tracked and true 
;	energy and momentum offsets are calculated.  The differences between 
;	the assigned and true values are returned by the function.
;	Calls X19C_COMM, X19C_BACK and SURFSCAT.
;	Calls ANGLE, CYLINTERCEPT, INTERCEPT, LINE_2PO, REFLECT_ELEM, VNORM, 
;	and VSCALP, from XOPER_LIB.
; MODIFICATION HISTORY:
;	Created 5-AUGUST-1992 by Mati Meron.
;-

    common x19c, ready, ori, x, y, z, inc, todee, rsam, rdet, totoff, beam, $
    nmo, nsp, monoc, spect, monoc_pars, spect_pars

    on_error, 1
    st = 0
    if n_elements(ready) eq 0 then message, 'Common blocks not initialized!'

    if keyword_set(kev) then wavl = 12.39842/wavl
    if wavl le todee then begin
	sphi = asin(wavl/todee)
	if keyword_set(deg) then talph = !dtor*alpha else talph = alpha
	tem = (sin(inc)*cos(2*sphi) - sin(talph))/(cos(inc)*sin(2*sphi))
	if abs(tem) le 1 then begin
	    dkn = Default(dkn,0.)
	    X19C_back, /save
	    X19C_comm, /ini, chi = asin(tem), phi = sphi
	    refbeam = Reflect_elem(beam,monoc(nmo))
	    horbeam = Project_elem(beam,spect(2),/comp)
	    sinp = Cylintercept(refbeam,spect(0),rsam,lonv = rlon,trav = rtra)
	    X19C_comm, two_theta = Angle(horbeam.dir,rtra), $
	    y_samp = Vscalp(spect(0).dir,rlon)
	    scabeam = Surfscat(refbeam, spect(2), dkt, dkn, $
	    /specrel, left = lef, extreme = ext, status = st)
	    if st eq 1 then begin
		dinp = Cylintercept(scabeam,spect(3),rdet,lonv=slon,trav=stra)
		X19C_comm, theta_det = Angle(rtra,stra), $
		y_det = Vscalp(spect(3).dir,slon)
		keep = [monoc_pars([2,3]),spect_pars([0,2,3,4])]
		X19C_back, /restore
		X19C_comm, chi = keep(0), phi = keep(1), two_theta = keep(2), $
		y_samp = keep(3), theta_det = keep(4), y_det = keep(5)
		refbeam = Reflect_elem(beam,monoc(nmo))
		sinp = Intercept(refbeam,spect(2))
		scabeam = Line_2po(sinp,dinp)
		specbeam = Reflect_elem(refbeam,spect(2))
		delk = scabeam.dir - specbeam.dir
		delktot = Vnorm(delk) - dkt
		delknor = Vscalp(delk,spect(2).dir) - dkn
		wavlreal = -todee*Vscalp(beam.dir,monoc(nmo).dir)
		if keyword_set(kev) then wavlreal = 12.39842/wavlreal
		delwavl = wavlreal/wavl - 1
	    endif else X19C_back, /restore
	endif
    endif

    if st eq 1 then return, [delwavl,delktot,delknor] else return, 1e38*[1,1,1]
end
