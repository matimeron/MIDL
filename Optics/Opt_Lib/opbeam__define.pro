Pro Opbeam__define

;+
; NAME:
;		OPBEAM__DEFINE
; VERSION:
;		5.6
; PURPOSE:
;		Defines the {OPBEAM} structure used in optics calculations and the
;		common block BEAM_STUFF.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None.
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
;		BEAM_STUFF.  See INIT_OPT for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard.  Defines the common block (if needed) and the structure
;		OPBEAM, including:
;			NAME	:	String, informational title.
;			COMMENT	:	string, additional info as needed.
;			SET		:	Integer, 1 for X only, 2 for Y only 3 for X and Y.
;			NPOINTS	:	Number of points, i.e. locations at which the beam is
;						being evaluated.  Maximal number given by NPOMAX (see
;						common block, currently 64.
;			WAVL	:	Float, wavelength in meters.
;			ELNAME	:	String array containing names of the optical elements.
;			XSEC	:	An array of 2*NPOMAX -1 BEAMSEC structures.  See
;						BEAMSEC__DEFINE for description.
;			YSEC	:	An array of 2*NPOMAX -1 BEAMSEC structures.  See
;						BEAMSEC__DEFINE for description.
;
;		Note 1:	Only the first 2*NPOINTS - 1 elements of LOC are being used.
;		Note 2:	All the even numbered locations in LOC contain optical elements.
;				All the odd-numbered contain FPR.
;
;		Calls OPT_INIT.
; MODIFICATION HISTORY:
;		Created 25-JUL-2001 by Mati Meron.
;		Modified 10-AUG-2005 by Mati Meron.  Internal changes only.
;		Modified 5-DEC-2006 by Mati Meron.  Internal changes only.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	Opt_init

	dum = {opbeam, name: '', comment: '', set: 0, npoints: 0l, wavl: 0., $
			elname: strarr(2*npomax-1), $
			xsec: replicate({beamsec},2*npomax-1), $
			ysec: replicate({beamsec},2*npomax-1)}

	return
end