Pro mba_go

	on_error, 1

	k = [2.723, 2.349, 2.033]


;	for i = 0l, n_elements(k)-1 do $
;	un_make, k[i], 0.028, !blpar.rgam, 84, 'apsmba_28mm_l*', $
;	!blpar.rsig, !blpar.asig, ang= [0.200,0.100], np= [100,50], /auto, /sho
	
	for i = 0l, n_elements(k)-1 do $
	un_make, k[i], 0.028, !blpar.rgam, 84, 'apsmba_28mm_s*', $
	!blpar.rsig, !blpar.asig, ang= [0.06,0.04], np= [60,40], /auto, /sho

return
end