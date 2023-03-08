Function Winsnip, nxy, qxy, arr, x_vector= xve, y_vector= yve, fullsize = ful

    on_error, 1

    if keyword_set(ful) then sxy = long(nxy) else sxy = 2*long(nxy) + 1
    lo = (sxy - 2*qxy)/2
    hi = (sxy - 1 + 2*qxy)/2
    res = reform(arr)
    siz = size(res)

    case 3*siz(0) + One_of(xve,yve) of
	3    :	begin
		    if siz(1) eq sxy(0) then res = res(lo(0):hi(0)) $
		    else message, 'wrong X size!'
		end
	4    :	begin
		    if siz(1) eq sxy(1) then res = res(lo(1):hi(1)) $
		    else message, 'wrong Y size!'
		end
	5    :	begin
		    if Arreq(siz(1:2),sxy) then res = $
		    res(lo(0):hi(0),lo(1):hi(1)) else message, 'wrong XY size!'
		end
	8	:	begin
				if siz[0] eq 3 and siz[1] ge 3 and Arreq(siz[2:3],sxy) $
				then res = res(*,lo(0):hi(0),lo(1):hi(1)) $
				else message, 'wrong XY size!'
			end
	else  :	message, 'bad input!'
    endcase

    return, res
end
