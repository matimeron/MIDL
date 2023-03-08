Pro Get_sordat, usnam, energy = enr, current = cur, $
    mrad = mr, ecrit = ecr, sigx = sx, sigy = sy, kval = kv, poles = pol

;+
; NAME:
;       GET_SORDAT
; PURPOSE:
;       Provides values of source parameters.
; CATEGORY:
;       X-ray calculations.
; CALLING SEQUENCE:
;       GET_SORDAT, USNAM [, keywords ]
; INPUTS:
;    USNAM
;       Name of a source.   The name must include two words, separated by
;	comma, tab or space, for example 'NSLS, X17'.  If corresponding to a
;	source that's listed in the lookup table (see routine LOAD_SOURCES for
;	details) the source parameters from the table will be substituted for
;	all the undefined keyword parameters.
; OPTIONAL INPUT PARAMETERS:
;       None.
; KEYWORD PARAMETERS:
;    ENERGY
;       Synchrotron energy, in GeV.
;    CURRENT
;       Synchrotron current, in Amp.
;    MRAD
;       Horizontal angular beam spread, in milirads.
;    ECRIT
;       Critical energy, in eV.
;    SIGX
;       Horizontal beam dimension (one sigma) in mm.
;    SIGY
;       Vertical beam dimension (one sigma) in mm.
;    KVAL
;	K parameter value.
;    POLES
;       Number of poles.
; OUTPUTS:
;       None.
; OPTIONAL OUTPUT PARAMETERS:
;       All keyword parameters act also as optional output parameters.
; COMMON BLOCKS:
;       SXR_STUFF.
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       GET_SORDAT can be used only after the source tables are loaded,
;       through a call to LOAD_SOURCES.
; PROCEDURE:
;       Parses USNAM and identifies the source in SORLIST (COMMON SXR_STUFF).
;	Then replaces all the UNDEFINED values of the keyword parameters with
;	the source parameters.
; MODIFICATION HISTORY:
;       Created 25-MARCH-1993 by Mati Meron.
;-

    common sxr_stuff, sorlist, abctab, enun, denun, curbeam

    on_error, 1

    if Strparse_mm(usnam,'	, ',par) ne 1 then message, 'Bad name!'
    nso = Strmatch_mm(par(0), sorlist.machine)
    if nso eq -1 then message, 'Machine ' + par(0) + ' not on list!'
    nli = Strmatch_mm(par(1), sorlist(nso).line(0:sorlist(nso).nlines).name)
    if nli eq -1 then message, 'Device '+par(1)+' not listed for '+ par(0)+ '!'

    enr = Default(enr, sorlist(nso).energy)
    cur = Default(cur, sorlist(nso).current)
    mr  = Default(mr, sorlist(nso).line(nli).mrad)
    ecr = Default(ecr, sorlist(nso).line(nli).ecrit)
    sx  = Default(sx, sorlist(nso).line(nli).sigx)
    sy  = Default(sy, sorlist(nso).line(nli).sigy)
    kv  = Default(kv, sorlist(nso).line(nli).kval)
    pol = Default(pol, sorlist(nso).line(nli).poles)

    return
end
