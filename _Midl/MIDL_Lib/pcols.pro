Pro Pcols, load = loa

;+
; NAME:
;		PCOLS
; VERSION:
;		7.08
; PURPOSE:
;		Creates a system variable named !PCOL.  !PCOL is a structure the fields
;		of which contain true color values as follows
;		!PCOL
;			BLACK
;			DRED
;			RED
;			LRED
;			DGREEN
;			GREEN
;			LGREEN
;			DBLUE
;			BLUE
;			LBLUE
;			YELLOW
;			PURPLE
;			CYAN
;			ORANGE
;			PINK
;			WHITE
;
;		All the values are long integers.
; CATEGORY:
;		Utility.
; CALLING SEQUENCE:
;		PCOLS
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/LOAD
;		Switch.  If set, sends a LOAD command to TRUCOL causing the colors
;		defined in !PCOL to be loaded into the current color table.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None
; PROCEDURE:
;		If !PCOL doesn't exist, it is created, else a listing of the colors is
;		displayed.  Uses TRUCOL from MIDL.
; MODIFICATION HISTORY:
;		Created 10-APR-2004 by Mati Meron.
;		Modified 25-DEC-2004 by Mati Meron.  Added keyword LOAD.
;		Modified 30-JUN-2008 by Mati Meron.  Added color WHITE.
;-

	rgb4 = [[0,0,0],[2,0,0],[4,0,0],$
			[4,2,2],[0,2,0],[0,4,0],$
			[2,4,2],[0,0,2],[0,0,4],$
			[2,2,4],[4,4,0],[4,0,4],$
			[0,4,4],[4,2,0],[4,0,2],[4,4,4]]
	cnams = ['black', 'dred', 'red', 'lred','dgreen','green','lgreen','dblue',$
			 'blue','lblue','yellow','purple', 'cyan','orange','pink','white']

	defsysv, '!pcol', exists = exs
	if not exs then begin
		comb = strcompress(strjoin(string(Trucol(rgb4,load=loa)) + 'l', ','))
		dum = execute('tem = create_struct(cnams,' + comb + ',name = "truco")')
		defsysv, '!pcol', tem, 1
	endif else begin
		if keyword_set(loa) then dum = Trucol(rgb4,/load)
		help, /st, !pcol
	endelse

	return
end