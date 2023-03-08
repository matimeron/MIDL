Pro Exres, dat

	on_error, 1

	siz = size(dat)
	if siz[1] eq 3 then begin
		case siz[0] of
			1	:	ddat = [dat,100*dat[2]/dat[1]]
			2	:	ddat = transpose($
					[[transpose(dat)],[100*reform(dat[2,*]/dat[1,*])]])
			else:	message, 'Not a valid 1D data!'
		endcase
	endif else message, 'Not a valid 1D data!'
	print, ddat

	return
end