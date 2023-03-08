Pro X19C_back, save = sav, restore = res

;+
; NAME:
;	X19C_BACK
; PURPOSE:
;	Saves or restores the contents of the common block X19C, created by
;	X19C_COMM.
; CATEGORY:
;	X-ray optics
; CALLING SEQUENCE:
;	X19C_SHOW {/SAVE or /RESTORE}
; INPUTS:
;	None
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    SAVE
;	Switch.  Specifies saving.	|    One and
;    RESTORE				|    only one
;	Switch.  Specifies restore.	|    must be set.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	Common block X19C.  For more info see routine X19C_COMM.
;	Common block BAX19C.  Contains similar entries to X19C, with the 
;	exception of ORI, X, Y, Z, NMO and NSP.
; SIDE EFFECTS:
;	On RESTORE action changes the contents of common X19c.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.
; MODIFICATION HISTORY:
;	Created 30-JULY-1992 by Mati Meron.
;-

    common x19c, ready, ori, x, y, z, inc, todee, rsam, rdet, totoff, beam, $
    nmo, nsp, monoc, spect, monoc_pars, spect_pars
    common bax19c, binc, btodee, brsam, brdet, btotoff, bbeam, $
    bmonoc, bspect, bmonoc_pars, bspect_pars

    on_error, 1
    sfl = keyword_set(sav)
    rfl = keyword_set(res)
    if not sfl xor rfl then message, 'Make up your mind!'
    if n_elements(ready) eq 0 then X19C_comm

    if sfl then begin
	binc = inc
	btodee = todee
	brsam = rsam
	brdet = rdet
	btotoff = totoff
	bbeam = beam
	bmonoc = monoc
	bspect = spect
	bmonoc_pars = monoc_pars
	bspect_pars = spect_pars
    endif else begin
	inc = binc
	todee = btodee
	rsam = brsam
	rdet = brdet
	totoff = btotoff
	beam = bbeam
	monoc = bmonoc
	spect = bspect
	monoc_pars = bmonoc_pars
	spect_pars = bspect_pars
    endelse

    return
end
