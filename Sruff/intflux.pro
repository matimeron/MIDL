Function Intflux, ev, use = usnam, ering = elgev, current = cur, $
    mrad = mr, ecrit = ecr, poles = npol, bmag = btes, $
    bandwidth = bw, wavelength = wav, kev_units = kev

;+
; NAME:
;	INTFLUX
; PURPOSE:
;	Calculates integrated flux for a SXR source.
; CATEGORY:
;	X-ray calculations.
; CALLING SEQUENCE:
;	Result = INTFLUX ( EV, keywords)
; INPUTS:
;    EV
;	Numeric, scalar or array, photon energy input (default units are eV).
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    USE
;	Name of a source to use in the calculation.  Optional.  If provided,
;	and corresponding to a source that's listed in the lookup table (see
;	routine LOAD_SOURCES for details) the source parameters from the table
;	will be used.  The name must include two words, separated by comma, tab
;	or space, for example 'NSLS, X17'.
;    ERING
;	Synchrotron energy, in GeV.		|    Unless the keyword USE
;    ECRIT					|    is used, two and
;	Critical energy, same units as EV.	|    ONLY two out of these
;    BMAG					|    three values
;	Magnetic field, in Tesla.		|    must be provided.
;    CURRENT
;	Synchrotron current, in Amp.
;    MRAD
;	Horizontal angular beam spread, in milirads.  Optional, default value
;	is 1., unless preempted by an explicit value from the table (through
;	the USE keyword).
;    POLES
;	Number of poles. Optional, default value is 1, unless preempted by an
;	explicit value from the table (through the USE keyword).
;    BANDWIDTH
;	The bandwidth to be used in the calculation.  If not provided, the
;	spectrum is calculated per unit energy instead of per given bandwidth.
;    WAVELENGTH
;	Switch.  Specifies that the input EV is wavelength(s) in Angstrems.
;    KEV
;	Switch.  Specifies that the input is energy in keV units.
; OUTPUTS:
;	Returns the calculated vertically integrated flux, in units of
;	photons/sec[/per bandwidth or per energy/wavelength unit].
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	The keyword USE can be used only after the source tables are loaded,
;	through a call to LOAD_SOURCES.
; PROCEDURE:
;	Uses standard formulas to calculate flux (see Jackson or Handbook of
;	Synchrotron radiation.  When the keyword USE is utilized, it is
;	important to remember that keyword parameters have precedence over
;	source data.  For example, if both USE and ECRIT are used in a call to
;	INTFLUX, the value of critical energy provided through ECRIT will be
;	used, not the original source parameter.
;
;	Uses calls to DEFAULT in MIDL.  Also calls EVUN, GET_SORDAT and BESELK_MM.
; MODIFICATION HISTORY:
;	Created 25-MARCH-1993 by Mati Meron.
;-

    on_error, 1
    ebeconv = 665.2
    mfac = 2.458e16

    usfl = n_elements(usnam) gt 0
    indat = total( $
	    ([n_elements(elgev),n_elements(ecr),n_elements(btes)] eq 1)*[1,2,4])

    case indat of
	3    : wecr = Evun(ecr, wavelength = wav, kev_units = kev)
	5    : wecr = ebeconv*btes*elgev^2
	6    : begin
		   wecr = Evun(ecr, wavelength = wav, kev_units = kev)
		   elgev = sqrt(wecr/(ebeconv*btes))
		end
	7    : message, 'Overspecified!'
	else : if not usfl then message, 'Underspecified!'
    endcase

    if usfl then Get_sordat, usnam, energy = elgev, current = cur, $
    mrad = mr, ecrit = wecr, poles = npol
    wev = Evun(ev, wavelength = wav, kev_units = kev)
    relen = wev/wecr

    scale = mfac*Default(cur,1.)*Default(mr,1.)*Default(npol,1)*elgev*relen
    if n_elements(bw) eq 0 then scale = scale/ev else scale = scale*bw

    return, scale*Beselk_mm(relen,5./3.,/int)
end
