When compiling sumNth.sac we get some warnings:
```
warning: ‘SACl_a__SSA0_1’ may be used uninitialized in this function [-Wmaybe-uninitialized]
1000 |         SAC_ND_WRITE (to_NT, to_pos) = expr; 
```
Using compiler version:
```
sac2c 1.3.3-MijasCosta-386-g66d20
```

Gianni Monteban & Martijn Vogelaar \
1047546 & 1047391