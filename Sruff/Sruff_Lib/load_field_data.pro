Pro Load_field_data, scu = scu, reload = rel, data = dat

;+
; NAME:
;		LOAD_FIELD_DATA
; VERSION:
;		8.72
; PURPOSE:
;		Loads magnetic field data from files saved in the .sav format in the
;		...\MIDL\Sruff\Data folder.
; CATEGORY:
;		Synchrotron calculations.
; CALLING SEQUENCE:
;		LOAD_FIELD_DATA [, keywords]
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS
; 	/SCU
; 		Switch.  Only relevant to the optional output parameter DATA.
; 	/RELOAD
; 		Switch.  Causes the field data to be reloaded.  Only needed if the data
; 		files changed without quitting IDL.
; 	DATA
; 		Optional output, see below.
; OUTPUTS:
;		None explicit, data put in common block.
; OPTIONAL OUTPUT PARAMETERS:
;	DATA
;		Returns either the permenent magnet data or (if /SCU is set) the 
;		superconducting magnet data, as a 3D array.  DATA[0,*,*] contains the
;		gaps, DATA[1,**] contains the periods and DATA[2,*,*] contains the
;		corresponding field values.
; COMMON BLOCKS:
;		FIELD_DATA.  Contains the following:
;			NFL		: Integer scalar.  1 if data has been loaded, 0 if not.
;			BPMU	: Permanent magnet field data as a 3D array.
;			BSCU	: Superconducting magnet field data as a 3D array.
;			BLIM	: Tabulated magnetic field limits in [low, high] format.
;			GLIM	: Tabulated gap limits in [low, high] format.
;			PLIM	: Tabulated period limits in [low high] format.
;			DIND	: Integer, specifies which data was accessed last. Value
;					  of 1 stands for permanent magnet, 2 for superconducting.
;			LAS		: Integer, specifies whether current call accesses same data
;					  as the previous one.  1 stands for "yes", 0 for "no".
;			ILAS	: Integer, used to block setting LAS to 1 until the data has
;					  been used by BGP_CONV at least once.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Checks to see if it has been previously called. If yes, it just returns
;		immediately, unless /RELOAD is set.
;		
;		Reads the data and fills the variables in the COMMON block.
;
;		Calls DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 1-JUN-2020 by Mati Meron.
;		Modified 15-JUL-2020 by Mati Meron.  Internal changes.
;-

	common field_data, nfl, bpmu, bscu, blim, glim, plim, dind, las, ilas
	on_error, 1

	if keyword_set(rel) then nfl = 0
	if Default(nfl,0) eq 0 then begin
		fname = getenv('sruff_data') + 'Beff_pmu.sav
		restore, fname
		bpmu = beff_pmu
		sname = getenv('sruff_data') + 'Beff_scu.sav
		restore, sname
		bscu = beff_scu
		nfl = 1
		dind = 0
		ilas = 0
	endif

	if keyword_set(scu) then begin
		dat = bscu
		ndind = 2
	endif else begin
		dat = bpmu
		ndind = 1
	endelse

	if ndind ne dind then begin
		blim = [min(dat[2,*,*],max=max),max]
		glim = [min(dat[0,*,*],max=max),max]
		plim = [min(dat[1,*,*],max=max),max]
		dind = ndind
		las = 0
	endif else las = ilas

	return
end