Function Brilliance, ev, use = usnam, ering = elgev, current = cur, $
    ecrit = ecr, sigx = sx, sigy = sy, poles = npol, bmag = btes, $
    bandwidth = bw, wavelength = wav, kev_units = kev

;+
; NAME:
;       BRILLIANCE
; PURPOSE:
;       Calculates on-axis brilliance for a SXR source.
; CATEGORY:
;       X-ray calculations.
; CALLING SEQUENCE:
;       Result = BRILLIANCE ( EV, keywords)
; INPUTS:
;    EV
;       Numeric, scalar or array, photon energy input (default units are eV).
; OPTIONAL INPUT PARAMETERS:
;       None.
; KEYWORD PARAMETERS:
;    USE
;       Name of a source to use in the calculation.  Optional.  If provided,
;       and corresponding to a source that's listed in the lookup table (see
;       routine LOAD_SOURCES for details) the source parameters from the table
;       will be used.  The name must include two words, separated by comma, tab
;       or space, for example 'NSLS, X17'.
;    ERING
;       Synchrotron energy, in GeV.             |    Unless the keyword USE
;    ECRIT                                      |    is used, two and
;       Critical energy, same units as EV.      |    ONLY two out of these
;    BMAG                                       |    three values
;       Magnetic field, in Tesla.               |    must be provided.
;    CURRENT
;       Synchrotron current, in Amp.
;    SIGX
;	Horizontal beam dimension (one sigma) in mm.
;    SIGY
;	Vertical beam dimension (one sigma) in mm.
;    POLES
;       Number of poles. Optional, default value is 1, unless preempted by an
;       explicit value from the table (through the USE keyword).
;    BANDWIDTH
;       The bandwidth to be used in the calculation.  If not provided, the
;       spectrum is calculated per unit energy instead of per given bandwidth.
;    WAVELENGTH
;       Switch.  Specifies that the input EV is wavelength(s) in Angstrems.
;    KEV
;       Switch.  Specifies that the input is energy in keV units.
; OUTPUTS:
;       Returns the calculated on axis brilliance in units of
;       photons/sec/mm^2/mrad^2[/per bandwidth or per eV].
; OPTIONAL OUTPUT PARAMETERS:
;       None.
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       The keyword USE can be used only after the source tables are loaded,
;       through a call to LOAD_SOURCES.
; PROCEDURE:
;       Calculates brilliance by BRILLIANCE = BRIGHTNESS/(2*PI*SIGX*SIGY).  
;	When the keyword USE is utilized, it is important to remember that 
;	keyword parameters have precedence over source data.  For example, if 
;	both USE and ECRIT are used in a call to BRILLIANCE, the value of 
;	critical energy provided through ECRIT will be used, not the original 
;	source parameter.
;
;	Uses calls to GET_SORDAT and BRIGHTNESS.
; MODIFICATION HISTORY:
;       Created 25-MARCH-1993 by Mati Meron.
;-

    if n_elements(usnam) gt 0 then Get_sordat, usnam, sigx = sx, sigy = sy

    return, Brightness( ev, use = usnam, ering = elgev, current = cur, $
    ecrit = ecr, poles = npol, bmag = btes, $
    bandwidth = bw, wavelength = wav, kev_units = kev)/(2*!pi*sx*sy)
end
