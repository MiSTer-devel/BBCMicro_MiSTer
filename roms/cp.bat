@echo off
rem #!/bin/bash
rem 
rem # Create the 512K ROM image
rem #
rem # This contains:
rem # 16x 16K ROMS images for the Model B
rem # 16x 16K ROMS images for the Master 128
rem 
rem IMAGE=tmp/rom_image.bin
rem 
rem rm -f $IMAGE
rem 
rem # Beeb ROM Images
rem 
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem 
rem # Note: It's not possible to pre-load the sideways RAM banks (4-7)
rem cat bbcb/os12.rom              >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem 
rem # Note: Bank 8 is special, in that B600-BFFF is mapped to RAM (for SWMMFS)
rem cat bbcb/swmmfs.rom            >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem 
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat bbcb/ram_master_v6.rom     >> $IMAGE
rem cat bbcb/basic2.rom            >> $IMAGE
rem 
rem # Master ROM Images
rem 
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat m128/adfs1-57.rom          >> $IMAGE
rem cat m128/mammfs.rom            >> $IMAGE
rem 
rem # Note: It's not possible to pre-load the sideways RAM banks (4-7)
rem cat m128/mos.rom               >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem cat generic/blank.rom          >> $IMAGE
rem 
rem cat generic/blank.rom          >> $IMAGE
rem cat m128/dfs.rom               >> $IMAGE
rem cat m128/viewsht.rom           >> $IMAGE
rem cat m128/edit.rom              >> $IMAGE
rem 
rem cat m128/basic4.rom            >> $IMAGE
rem cat m128/adfs.rom              >> $IMAGE
rem cat m128/view.rom              >> $IMAGE
rem cat m128/terminal.rom          >> $IMAGE

copy /b bbcb\os12.rom +bbcb\swmmfs2.rom +bbcb\ram_master_v6.rom +bbcb\basic2.rom +m128\adfs1-57.rom +m128\mammfs2.rom +m128\mos.rom +m128\dfs.rom  +m128\viewsht.rom +m128\edit.rom +m128\basic4.rom +m128\adfs.rom +m128\view.rom +m128\terminal.rom rom.bin
rem copy /b bbcb\os12.rom +bbcb\swmmfs2.rom +bbcb\ram_master_v6.rom +bbcb\basic2.rom +m128\adfs1-57.rom +m128\mammfs2.rom +m128\mos.rom +m128\dfs0.9.rom +m128\dfs0.9.rom  +m128\viewsht.rom +m128\edit.rom +m128\basic4.rom +m128\adfs.rom +m128\view.rom +m128\terminal.rom rom.bin
srec rom.bin -binary -o rom.mif -mif 8
pause
