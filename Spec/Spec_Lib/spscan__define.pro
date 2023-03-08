Pro Spscan__define

;+
; NAME:
;		SPSCAN__DEFINE
; VERSION:
;		8.13
; PURPOSE:
;		Defines the {SPSCAN} sub-structure used with SPEC files.
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
;		Standard.  Defines the structure SPSCAN, including:
;
;			STAT	:	Scan status indicator, with the possible values of:
;							0	:	Nonexistant scan.
;							1	:	Scan OK.
;							2	:	No data present.
;							3	:	Scan may be incomplete.
;							5	:	Data_header - data mismatch.
;							7	:	Illegitimate (NaN) data present.
;			SHEAD	:	The header line of the scan, string.
;			STYPE	:	Scan type, string.
;			SDATIME	:	Scan date and time, string.
;			SCOMMENT:	Possible comment, string.
;			PTR		:	An N_S-element long vector.  Includes, in order, the
;						locations in the file of:
;							0		:	First line of scan (#S).
;							1		:	Data header line (#L).
;							2		:	First line of data.
;							3		:	First line of linear detector data,
;									:	where applicable.
;							4-N_S-2	:	Currently not used.
;							N_S - 1 :	Last line of scan.
;			LPTR	:	An N_S-element long vector.  Includes, in order, the
;						line numbers, within the scan, of:
;							0		:	First line of scan (#S).
;							1		:	Data header line (#L).
;							2		:	First line of data.
;							3		:	First line of linear detector data,
;									:	where applicable.
;							4-N_S-2	:	Currently not used.
;							N_S - 1 :	Last line of scan.
;			PPLEN	:	Total number of entries in the #P field.  For internal
;						use only.
;			PP_SET	:	An integer vector of length N_M.  Values of 1 correspond
;						to currently defined local parameters (within the
;						tracked list), 0-s to undefined.
;			PP_IND	:	A long vector of length N_M.  Indices of the values of
;						currently defined local parameters (within the #P field)
;			PP_VAL	:	A float vector of length N_M.  Values of the #P
;						parameters (see SPFILE__DEFINE).
;			SL_SET	:	An integer vector of length N_S.  Values of 1 correspond
;						to currently defined slits (within the tracked list),
;						0-s to undefined.  Values of 3 represent "partially
;						defined" slits, vertical only, and values of 5 represent
;						horizontal only slits.
;			SL_IND	:	A [4,N_S] long array.  Indices of the values of the
;						currently defined slits (within the #P field).
;			SL_VAL	:	A [4,N_S] float array.  Each column contains the
;						positions of one slit, in a TBLR order.
;			ANGS	:	A 4-element floating vector.  The first 3 elements
;						contain, in order, the strating values of Alpha, Beta
;						and Dth for the scan.  Last element is currently unused.
;			VARAN	:	A 4-element integer vector, specifying whether the
;						angles from the ANGS field are constant or varying.
;						Value of 1 means "varying", 0 means constant.
;			OUTMODE	:	Long integer value specifying detector arm setting.
;						Possible values are:
;							0	:	No information available.
;							1	:	Generic (old) setting.
;							3	:	X-ray reflectivity.
;							5	:	GID setting.
;							6	:	Mix of reflectivity and GID, error state.
;			ABSO	:	Absorber type, string.
;			NABSO	:	Number of absorbers, long.
;			TABSO	:	Absorber thickness, in micron, float.
;			MIRFL	:	Bimorph mirror data status, 0 - absent, 1- present.
;			MIRVAL	:	Bimorph mirror voltages, 16-element floating vector.
;			G_L		:	A 4-element floating vector.  G_L[0] is not used,
;						G_L[1:3] contains the lengths G_L1, G_L2 and G_L3, in
;						order.
;			Q_VAL	:	A 4-element floating vector.  Q_VAL[0] is not used,
;						Q_VAL[1:3] contains the [H,K,L] elements of the
;						Q vector.
;			NCR		:	A 2-element long vector, includes, in order, number
;						of columns and rows in the data.
;			PEAK_LOC:	Peak location.			|
;			PEAK_I	:	Intensity at peak.		|	All these are
;			COM		:	Peak center-of-mass.	|	read from the
;			FWHM	:	Peak FWHM.				|	SPEC file, not
;			CEN		:	Peak center.			|	calculated.
;			LAMBDA	:	Wavelength.
;			LINDET	:	Linear detector flag, with the possible values of:
;							0	:	No LD.
;							1	:	LD OK.
;							2	:	LD exist, no data present.
;							3	:	LD data exists but may be incomplete.
;			CH_IND	:	Long scalar, the index of the number of channels for the
;						linear detector (if present) within the #P field.
;			CHAN	:	Number of linear detector channels, long.
;			CHRO	:	Number of lin. det data rows, long.
;			LDIST	:	Distance from Sample to linear detector, in mm.
;			LMPC	:	Channel spacing (mm per channel).
;			LNS		:	Specular channel.
;			MCSTAT	:	MCA flag, with the possible values of:
;							0	:	No MCA.
;							1	:	MCA OK.
;							2	:	MCA defined, no data present.
;							3	:	MCA data present, may be incomplete.
;			MCNUM	:	Number of MCA spectra in scan, long.  0 means no MCA.
;			MCHAN	:	Number of channels per MCA spectrum, long.
;			MCHRO	:	Number of data rows per MCA spectrum, long.
;			MCSLP	:	Slope, measurement units (keV, mm ...) per channel.
;			MCINT	:	Calibration intercept in measurement units.
;			MCTIM	:	A [2, N_L] float array containing the live and clock
;						times for the MCA spectra present.  The pair MCTIM[*,i]
;						consists of live and clock times for spectrum #I+1.
;			MCPTR	:	A N_L elements long vector, containing the locations
;						(within the file) of the first lines of all MCA spectra
;						in the scan.
;			MCLPTR	:	A N_L elements long vector, containing the line numbers
;						(within the scan) of the first lines of all MCA spectra
;						in the scan.
;			PDSTAT	:	AD (Area Detector) flag with the possible values of:
;							0	:	No AD.
;							1	:	AD defined but AD constants are missing.
;							2	:	AD defined but no data present.
;							3	:	Pilatus area detector defined.
;							5	:	APEX area detector defined.
;							7	:	Camera defined.
;							9	:	Currently not used.
;							11	:	Pilatus 1M area detector defined.
;			PDFNAM	:	Name of the first AD file saved for the current scan.
;			PDFEXT	:	The file extension for the AD files.
;			PDDIM	:	Two element vector (long), the x and y dimensions of
;						an Area Detector (AD) in pixel units.
;			PDPIX	:	Float constant, the AD pixel size (in mm).
;			PDXC	:	X coordinate of the center (corresponding to direct
;						beam) AD pixel.
;			PDYC	: 	Same as ADXC, for Y-coordinate.
;			PDIST	:	Sample-AD distance, in mm.
;			PDSDIST	:	S4-AD distance, in mm.
;			PDHV	:	AD orientation, 0 for horizontal, 1 for vertical. 
;						Meaningless for APEX, taken as 1 by default.
;			PDFAR	:	Long integer flag, set to 1 for "far area detector",
;						0 otherwise.
;
;		Note:	For current values of N_S, N_M, N_L, see SPEC_FILE_INFO.
; MODIFICATION HISTORY:
;		Created 30-AUG-2002 by Mati Meron.
;		Modified 25-MAR-2003 by Mati Meron.  Added tags LINDET and CHAN.
;		Modified 30-MAR-2003 by Mati Meron.  Reorganized.
;		Modified 1-JUL-2003 by Mati Meron.  Reorganized again.
;		Modified 25-APR-2004 by Mati Meron.  Expanded to include MCA data.
;		Modified 10-AUG-2007 by Mati Meron.  Expanded to include PD (Pilatus
;		Detector) data.
;		Modified 10-FEB-2008 by Mati Meron.  Added the OUTMODE field.
;		Modified 15-AUG-2008 by Mati Meron.  Added the ANGLES and VARANG fields.
;		Modified 20-MAR-2010 by Mati Meron.  Added the fields PDDIM andd PDPIX 
;		(moved over from SPFILE) and changed some definitions. 
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-FEB-2011 by Mati Meron.  Added field PDFAR.
;		Modified 10-OCT-2011 by Mati Meron.  Added field PDFEXT.
;		Modified 25-OCT-2011 by Mati Meron.  Adeed fields MIRFL and MIRVAL.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	dum = {spscan, $
		stat: 0l, shead: '', stype: '', sdatime: '', scomment: '', $
		ptr: lonarr(n_s), lptr: lonarr(n_s), pplen: 0l, $
		pp_set: intarr(n_m), pp_ind: lonarr(n_m), pp_val: fltarr(n_m), $
		sl_set: intarr(n_s), sl_ind: lonarr(4,n_s), sl_val: fltarr(4,n_s), $
		angs: fltarr(4), varan: intarr(4), outmode: 0l, $
		abso: '', nabso: 0l, tabso: 0., mirfl: 0l, mirval: fltarr(16), $
		g_l: fltarr(4), q_val: fltarr(4), ncr: lonarr(2), $
		peak_loc: 0., peak_I: 0., COM: 0., FWHM: 0., CEN: 0., LAMBDA: 0., $
		lindet: 0l, ch_ind: 0l, chan: 0l, chro: 0l, ldist:0., lmpc:0., lns:0l, $
		mcstat: 0l, mcnum: 0l, mchan: 0l, mchro: 0l, mcslp: 0., mcint: 0., $
		mctim: fltarr(2,n_l), mcptr: lonarr(n_l), mclptr: lonarr(n_l), $
		pdstat: 0l, pdfnam: '', pdfext: '', pddim: lonarr(2), pdpix: 0., $
		pdxc: 0l, pdyc: 0l, pdist: 0., pdsdist: 0., pdhv: 0l, pdfar: 0l}

	return
end