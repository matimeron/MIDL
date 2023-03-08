Function Surfscat, ray, surf, dkt, dkn, specrel = spr, raynor = spn, $
    left = lef, extreme = ext, status = st, name = nam, idnum = id

;+
; NAME:
;	SURFSCAT
; PURPOSE:
;	Calculates the result of surface scattering of an X-ray beam.
; CATEGORY:
;	X-ray optics.
; CALLING SEQUENCE:
;	Result = SURFSCAT (RAY, SURF, DKT, DKN [, keywords ])
; INPUTS:
;    RAY
;	A GELEM structure representing a line.
;    SURF
;	A GELEM structure representing a plane.
;    DKT
;	Relative total momentum change (delta(k)/k).  Interpretation depends
;	on keywords.
;    DKN
;	Relative normal momentum change ((delta(k).normal)/k).  Interpretation
;	depends on keywords.  If the keyword EXTREME is set, DKN becomes an
;	output parameter.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    SPECREL
;	Switch.  If set, DKT and DKN are taken relative to the specularly
;	reflected beam.  Default is relative to the incoming beam.
;    RAYNOR
;	Switch.  If set, the normal direction (for DKN) is along the normal to
;	the beam (as selected by SPECREL), in the scattering plane.  Default is
;	the normal to the surface.
;    LEFT
;	Switch.  If set, the directions of RAY, SURF and Result form a left
;	handed triple.  Default is right handed.
;    EXTREME
;	Accepts one of the character values 'UP', 'SIDE' or 'DOWN', and directs
;	the momentum change appropriately.  If specified, DKN is ignored.
;    STATUS
;	Optional output parameter, see below.
;    NAME
;	Accepts a character string that is used as the NAM field of the result.
;	Default is "Line".
;    IDNUM
;	Accepts a number that is used as the IDN field of the result.
;	Default is 1.
; OUTPUTS:
;	Returns the scattered beam in the form of a GELEM line structure.
;	In case of failure returns and "Undefined" GELEM structure.
; OPTIONAL OUTPUT PARAMETERS:
;    STATUS
;	The name of a variable to accept the status code of the operation,
;	1 for success, 0 for failure.
;    DKN
;	If the keyword EXTREME is set, DKN returns the internally calculated
;	value of the relative normal momentum change.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	RAY and SURF must be proper GELEM structures.
; PROCEDURE:
;	Transforms the incoming to the outgoing beam through a combination of
;	reflection and rotation operations.  If DKT and DKN are geometrically
;	impossible, declares failure, sets STATUS to 0 and returns and
;	"Undefined" result.
;	Calls MAKE_ELEM, REFLECT_ELEM, ROTATE_ELEM, MAKE_LINE, INTERCEPT,
;	UNIVEC and VSCALP, all from XOPER_LIB.  Also calls TYPE and STRMATCH_MM
;	from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JULY-1992 by Mati Meron.
;-

    on_error, 1
    extopts = ['UP', 'SIDE', 'DOWN']
    st = 0

    if Type(ray) ne 8 then message, 'Ray must be a GELEM line!' else $
    if ray.dim ne 1 then message, 'Ray must be a line!'
    if Type(surf) ne 8 then message, 'Surface must be a GELEM plane!' else $
    if ray.dim ne 1 then message, 'Surface must be a plane!'

    if keyword_set(spr) then res = Reflect_elem(ray,surf) else res = ray
    sna = Vscalp(res.dir,surf.dir)
    csa = sqrt(1. - sna^2)

    st = 1
    qt = abs(dkt/2.)
    if qt le 1 then begin
	psi = acos(1 - 2*qt^2)
	denom = qt*sqrt(1 - qt^2)
	if n_elements(ext) eq 0 then begin
	    qn = dkn/2.
	    if keyword_set(spn) then cphi = qn/denom else $
	    cphi = (qn + sna*qt^2)/(denom*csa)
	    if abs(cphi) le 1 then phi = acos(cphi) else st = 0
	endif else begin
	    ndir = Strmatch_mm(ext,extopts,2)
	    if ndir ge 0 then begin
		phi = ndir*!pi/2
		if not keyword_set(spn) then begin
		    if ndir eq 1 then begin
			qn = 0.
			cphi = sna*qt^2/(denom*csa)
			if abs(cphi) le 1 then phi = acos(cphi) else st = 0
		    endif else qn = cos(phi)*denom*csa - sna*qt^2
		endif else qn = cos(phi)*denom
		dkn = 2*qn
	    endif else st = 0
	endelse
    endif else st = 0

    if st then begin
	phi = (1 - 2*keyword_set(lef))*phi
	sloc = Intercept(res,surf)
	lvc = Univec(crossp(res.dir,surf.dir))
	mvc = crossp(lvc,res.dir)
	rax = Make_line(sloc.loc, cos(phi)*lvc - sin(phi)*mvc)
	return, Rotate_elem(res, rax, psi, name = nam, idnum = id)
    endif else return, Make_elem(-1,[0,0,0],[0,0,0])

end
