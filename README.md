# kismet decompilation experiments

```
cargo run --release -- disassemble fsd_cd2.json -o cfg

-f _MENU_ServerList_C:SetSearchDistance // simple loop
-f ModioModBrowserWidget_C:OnKeyDown // simple condition

-f SandboxUtilities_C:CreateSandbox // 2 nested loops
-f CD2_EC_Spawner_C:SetupValues
-f LoadDefaultProjectiles
-f Loadout.Loadout_C:GetClass // switch
-f ModdedSavegameCheck
-f SCREEN_MissionSelectionMK3_C:SelectRandomMission // 3 nested loops
-f JSONValue_C:ToString // giga nested
-f CD2_Mutator_Manager_C:MakeRandIntervalsValue // GIGA
-f SU_MaterialSlot_C:GetMaterials // jump edge case

-f Darkness_C:ValueOrDefault
-f DuringPECountdown_C:HandleOption
-f RandomChoicePerMission_C:Validate
-f ITM_LoadoutIconSelector_C:FillGrid
-f ITM_HazPlusItem_C:GetAmountFromManager
```
