Pro Opt_init

;+
; NAME:
;		OPT_INIT
; VERSION:
;		5.6
; PURPOSE:
;		Initializes the common block BEAM_STUFF, used in optical calculations.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		OPT_INIT
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		BEAM_STUFF.  Includes conversion factors from external units
;		(Angstrem, micron-microradian) to internal (meter, meter-radian).
;		Specifically
;			1) EXS		:	Flag, set to 1 when common block is defined.
;			2) NPOMAX	:	Maximal number of points allowed.
;			3) MMICR	:	Conversion from micro to MKS
;			4) MWLEN	:	Conversion from Angstrem to meter.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Defines the common block.
; MODIFICATION HISTORY:
;		Created 10-AUG-2005 by Mati Meron as INIT_OPT.
;		Modified 5-DEC-2006 by Mati Meron.  Renamed to OPT_INIT for consistency.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	if Type(exs) eq 0 then begin
		npomax = 64l
		mmicr = 1e-6
		mwlen = 1e-10
		exs = 1
	endif

	return
end