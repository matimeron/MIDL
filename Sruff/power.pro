Function Power, ev, spec, lims, watt = wat, $
    bandwidth = bw, wavelength = wav, kev_units = kev

;+
; NAME:
;	POWER
; PURPOSE:
;	Integrates of photon spectrum.
; CATEGORY:
;	X-ray calculations.
; CALLING SEQUENCE:
;	Result = POWER ( EV, SPEC, [, LIMS] [, keywords])
; INPUTS:
;    EV
;	Numeric, scalar or array, photon energy input (default units are eV).
;    SPEC
;	Photon spectrum, same length as EV
; OPTIONAL INPUT PARAMETERS:
;    LIMS
;	A two element vector, specifies integration limits.  Default is the
;	whole spectrum.
; KEYWORD PARAMETERS:
;    WATT
;	Switch.  If set, the function calculates integrated power in watts.
;	Default is calculating number of photons.
;    BANDWIDTH
;	The bandwidth to be used in the calculation.  If not provided, the 
;	spectrum is assumed to be given per unit energy, in units specified by
;	the other keywords.
;    WAVELENGTH
;	Switch.  Specifies that EV is in wavelength units (Angstrem).
;    KEV
;	Switch.  Specifies that EV is in keV units.
; OUTPUTS:
;	Returns either the total number of photons or (if WATT is set) the 
;	total power within the integration range.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	The length of EV and SPEC should be 4 or larger.
; PROCEDURE:
;	Standard Simpson integration plus (optionally) unit conversion.
;	Uses calls to ONE_OF and INTEG in MIDL.
; MODIFICATION HISTORY:
;	Created 1-APRIL-1993 by Mati Meron.
;-

    on_error, 1
    econv = 12398.
    pconv = 1.6022e-19

    dlims = ev([0, n_elements(ev) - 1])
    if dlims(0) gt dlims(1) then begin
	dlims = reverse(dlims)
	inev = reverse(ev)
	inspec = reverse(spec)
    endif else begin
	inev = ev
	inspec = spec
    endelse

    if n_elements(lims) eq 2 then begin
	if lims(0) gt lims(1) then lims = reverse(lims)
	lims(0) = lims(0) > dlims(0)
	lims(1) = lims(1) < dlims(1)
    endif else lims = dlims

    p = (where(inev le lims(0), nw))(nw - 1)
    q = (where(inev ge lims(1), nw))(0)
    l = q - p
    inev = 1.*inev(p:q)
    inspec = inspec(p:q)
    if n_elements(bw) gt 0 then inspec = inspec/(bw*inev)

    if keyword_set(wat) then begin
	case (1 + One_of(kev,wav)) of
	    0:	inspec = pconv*inspec*inev
	    1:	inspec = 1000.*pconv*inspec*inev
	    2:	inspec = econv*pconv*inspec/inev
	endcase
    endif

    a = (lims(0) - inev(0))/(inev(1) - inev(0))
    b = (inev(l) - lims(1))/(inev(l) - inev(l-1))

    return, (1-a-b)*Integ(inev,inspec,/val) + $
    a*Integ(inev(1:l),inspec(1:l),/val) +b*Integ(inev(0:l-1),inspec(0:l-1),/val)
end
