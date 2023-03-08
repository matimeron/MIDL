Pro Spfile__define

;+
; NAME:
;		SPFILE__DEFINE
; VERSION:
;		8.12
; PURPOSE:
;		Defines the {SPFILE} sub-structure used with SPEC files.
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
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard.  Defines the structure SPFILE, including:
;
;			STATUS	:	File status indicator, with the possible values of:
;							0	:	Nonexistant file.
;							1	:	File OK.
;							2	:	No data present.
;							3	:	Data exists but file may be corrupted.
;			NAME	:	File name, string.
;			SIZE	:	File size, in bytes, long.
;			DATIME	:	Creation date and time, string, read from the SPEC file.
;			COMMENT	:	Possible comment, string.
;			PDDIM	:	Two element vector (long), the x and y dimensions of
;						an Area Detector (AD) in pixel units.
;			PDPIX	:	Float constant, the AD pixel size (in mm).
;			MCAFL	:	Integer flag, value of 1 signifies MCA data.
;			PDFL	:	Integer flag, possible values of:
;							0	:	No area detector data present in file.
;							3	:	PD (Pilatus) data present.
;							5	:	APEX data present.
;							7	:	Camera present.
;							9	:	Currently not used.
;							11	:	Pilatus 1M area detector present.
;			PDPATH	:	The directory path for the PD and/or APEX data (if 
;						defined).
;			NZP		:	Number of global parameters (read from the file header
;						#Z field) present.
;			ZP_NAM	:	A string vector of length N_S.  Global parameter names.
;			ZP_VAL	:	A float vector of length N_S. Global parameters' values.
;			NPP		:	Number of the tracked #P parameters.
;			PP_NAM	:	A string vector of length N_M.  Names of the tracked #P
;						parameters.
;			PP_EXS	:	An integer array, indicating which of the parameters on
;						the PP_NAM list exist somewhere (though not necessarily
;						everywhere) in the file.  Value of 1 indicates "exists".
;			PP_AN	:	An integer array containing the indices of the angle
;						parameters, IN_ROT, OUT_ROT, DET_TH, in the PP_NAM list.
;			PP_GX	:	An integer [2,2] array containing the indices of the
;						"special" parameters: OUT_HTG, OUT_ROTG, OUT_HTX,
;						OUT_ROTX, in the PP_NAM list.
;			NSL		:	Number of tracked slits.
;			SL_NAM	:	A string vector of length N_S. Slit names.
;			SL_EXS	:	An integer array, indicating which of the slits on the
;						SL_NAM list exist somewhere (though not necessarily
;						everywhere) in the file.  Value of 1 indicates "exists".
;						Values of 3 represent "partially defined" slits,
;						vertical only, and values of 5 represent horizontal
;						only slits.
;			NSCAN	:	Number of scans in file, long.
;			SCAN	:	An array of N_SMAX structures of type SPSCAN.  See
;						SPSCAN__DEFINE for details.
;
;		Note:	For current values of N_S, N_M, N_L, N_SMAX, see SPEC_FILE_INFO.
; MODIFICATION HISTORY:
;		Created 30-AUG-2002 by Mati Meron.
;		Modifed 30-MAR-2003 by Mati Meron.  Reorganized.
;		Modified 1-JUL-2003 by Mati Meron.  Reorganized again.
;		Modified 10-AUG-2007 by Mati Meron.  Added PD (Pilatus) fields.
;		Modified 10-FEB-2008 by Mati Meron.  Added the PP_GX field.
;		Modified 15-AUG-2008 by Mati Meron.  Added the PP_AN field.
;		Modified 20-MAR-2010 by Mati Meron.  Slight definition changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-OCT-2011 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	dum = {spfile, $
		status: 0l, name: '', size: 0l, datime: '', comment: '', $
		pddim: lonarr(2), pdpix: 0., mcafl: 0, pdfl: 0, pdpath: '', $
		nzp: 0l, zp_nam: strarr(n_s), zp_val: fltarr(n_s), $
		npp: 0l, pp_nam: strarr(n_m), pp_exs: intarr(n_m), $
		pp_an: intarr(4), pp_gx: intarr(2,2), $
		nsl: 0l, sl_nam: strarr(n_s), sl_exs: intarr(n_s), $
		nscan: 0l, scan: replicate({spscan},n_smax)}

	return
end