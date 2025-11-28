Begin Object Class=/Script/UMG.VerticalBox Name="MissionPanel"
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_10"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_9"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_6"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_8"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_13"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_5"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_12"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_1"
   End Object
   Begin Object Class=/Script/UMG.VerticalBoxSlot Name="VerticalBoxSlot_15"
   End Object
   Begin Object Name="VerticalBoxSlot_10"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=Button'"ButtonCreateSandbox"'
   End Object
   Begin Object Name="VerticalBoxSlot_9"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=Button'"ButtonSpawnScriptedWave"'
   End Object
   Begin Object Name="VerticalBoxSlot_6"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=Button'"EnableMuleButton"'
   End Object
   Begin Object Name="VerticalBoxSlot_8"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=Button'"PrintDropPodLocation"'
   End Object
   Begin Object Name="VerticalBoxSlot_13"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=HorizontalBox'"HorizontalBox_0"'
   End Object
   Begin Object Name="VerticalBoxSlot_5"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=HorizontalBox'"HorizontalBox_1"'
   End Object
   Begin Object Name="VerticalBoxSlot_12"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=HorizontalBox'"HorizontalBox_3"'
   End Object
   Begin Object Name="VerticalBoxSlot_1"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=HorizontalBox'"HorizontalBox_4"'
   End Object
   Begin Object Name="VerticalBoxSlot_15"
      Padding=(Left=10.000000,Top=10.000000,Right=10.000000)
      Parent=VerticalBox'"MissionPanel"'
      Content=NamedSlot'"NamedSlotSpawnEnemies"'
   End Object
   Slots(0)=VerticalBoxSlot'"VerticalBoxSlot_10"'
   Slots(1)=VerticalBoxSlot'"VerticalBoxSlot_6"'
   Slots(2)=VerticalBoxSlot'"VerticalBoxSlot_8"'
   Slots(3)=VerticalBoxSlot'"VerticalBoxSlot_9"'
   Slots(4)=VerticalBoxSlot'"VerticalBoxSlot_13"'
   Slots(5)=VerticalBoxSlot'"VerticalBoxSlot_12"'
   Slots(6)=VerticalBoxSlot'"VerticalBoxSlot_5"'
   Slots(7)=VerticalBoxSlot'"VerticalBoxSlot_1"'
   Slots(8)=VerticalBoxSlot'"VerticalBoxSlot_15"'
   bExpandedInDesigner=True
   DisplayLabel="MissionPanel"
End Object
Begin Object Class=/Script/UMGEditor.WidgetSlotPair Name="WidgetSlotPair_0"
   WidgetName="MissionPanel"
End Object
Begin Object Class=/Script/UMG.Button Name="ButtonCreateSandbox"
   Begin Object Class=/Script/UMG.ButtonSlot Name="ButtonSlot_0"
   End Object
   Begin Object Name="ButtonSlot_0"
      Parent=Button'"ButtonCreateSandbox"'
      Content=TextBlock'"TextBlock_9"'
   End Object
   Slots(0)=ButtonSlot'"ButtonSlot_0"'
   DisplayLabel="ButtonCreateSandbox"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_9"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "BB7148FF3A9C42B192309F8220906EC9", "Create sandbox")
   ColorAndOpacity=(SpecifiedColor=(R=0.000000,G=0.000000,B=0.000000,A=1.000000))
   bExpandedInDesigner=True
   DisplayLabel="TextBlock_9"
End Object
Begin Object Class=/Script/UMG.Button Name="EnableMuleButton"
   Begin Object Class=/Script/UMG.ButtonSlot Name="ButtonSlot_0"
   End Object
   Begin Object Name="ButtonSlot_0"
      Parent=Button'"EnableMuleButton"'
      Content=TextBlock'"TextBlock_6"'
   End Object
   Slots(0)=ButtonSlot'"ButtonSlot_0"'
   bExpandedInDesigner=True
   DisplayLabel="EnableMuleButton"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_6"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "44D23EF99C9B4DEBBC191B36E9B4DA7E", "Enable drop pod button on mule")
   ColorAndOpacity=(SpecifiedColor=(R=0.000000,G=0.000000,B=0.000000,A=1.000000))
   DisplayLabel="TextBlock_6"
End Object
Begin Object Class=/Script/UMG.Button Name="PrintDropPodLocation"
   Begin Object Class=/Script/UMG.ButtonSlot Name="ButtonSlot_0"
   End Object
   Begin Object Name="ButtonSlot_0"
      Parent=Button'"PrintDropPodLocation"'
      Content=TextBlock'"TextBlock_7"'
   End Object
   Slots(0)=ButtonSlot'"ButtonSlot_0"'
   bExpandedInDesigner=True
   DisplayLabel="PrintDropPodLocation"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_7"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "33E716056CA446188FD0D551AFF58E7E", "Call drop pod")
   ColorAndOpacity=(SpecifiedColor=(R=0.000000,G=0.000000,B=0.000000,A=1.000000))
   DisplayLabel="TextBlock_7"
End Object
Begin Object Class=/Script/UMG.Button Name="ButtonSpawnScriptedWave"
   Begin Object Class=/Script/UMG.ButtonSlot Name="ButtonSlot_0"
   End Object
   Begin Object Name="ButtonSlot_0"
      Parent=Button'"ButtonSpawnScriptedWave"'
      Content=TextBlock'"TextBlock_8"'
   End Object
   Slots(0)=ButtonSlot'"ButtonSlot_0"'
   bExpandedInDesigner=True
   DisplayLabel="ButtonSpawnScriptedWave"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_8"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "BAA2854FE20847038A69AE5A9AC4035E", "Spawn wave")
   ColorAndOpacity=(SpecifiedColor=(R=0.000000,G=0.000000,B=0.000000,A=1.000000))
   bExpandedInDesigner=True
   DisplayLabel="TextBlock_8"
End Object
Begin Object Class=/Script/UMG.HorizontalBox Name="HorizontalBox_0"
   Begin Object Class=/Script/UMG.HorizontalBoxSlot Name="HorizontalBoxSlot_1"
   End Object
   Begin Object Class=/Script/UMG.HorizontalBoxSlot Name="HorizontalBoxSlot_0"
   End Object
   Begin Object Name="HorizontalBoxSlot_1"
      Size=(SizeRule=Fill)
      Parent=HorizontalBox'"HorizontalBox_0"'
      Content=Button'"ButtonRevive"'
   End Object
   Begin Object Name="HorizontalBoxSlot_0"
      Size=(SizeRule=Fill)
      Parent=HorizontalBox'"HorizontalBox_0"'
      Content=Button'"ResupplyAmmoButton"'
   End Object
   Slots(0)=HorizontalBoxSlot'"HorizontalBoxSlot_0"'
   Slots(1)=HorizontalBoxSlot'"HorizontalBoxSlot_1"'
   bExpandedInDesigner=True
End Object
Begin Object Class=/Script/UMG.Button Name="ResupplyAmmoButton"
   Begin Object Class=/Script/UMG.ButtonSlot Name="ButtonSlot_0"
   End Object
   Begin Object Name="ButtonSlot_0"
      Parent=Button'"ResupplyAmmoButton"'
      Content=TextBlock'"TextBlock_21"'
   End Object
   Slots(0)=ButtonSlot'"ButtonSlot_0"'
   bExpandedInDesigner=True
   DisplayLabel="ResupplyAmmoButton"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_21"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "64E450B5356F42798D6750960EAC2B0F", "Resupply/Heal")
   ColorAndOpacity=(SpecifiedColor=(R=0.000000,G=0.000000,B=0.000000,A=1.000000))
   bExpandedInDesigner=True
   DisplayLabel="TextBlock_21"
End Object
Begin Object Class=/Script/UMG.Button Name="ButtonRevive"
   Begin Object Class=/Script/UMG.ButtonSlot Name="ButtonSlot_0"
   End Object
   Begin Object Name="ButtonSlot_0"
      Parent=Button'"ButtonRevive"'
      Content=TextBlock'"TextBlock_4"'
   End Object
   Slots(0)=ButtonSlot'"ButtonSlot_0"'
   bExpandedInDesigner=True
   DisplayLabel="ButtonRevive"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_4"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "5F9607149B1A4E3587A0478D36BC60A3", "Revive self")
   ColorAndOpacity=(SpecifiedColor=(R=0.000000,G=0.000000,B=0.000000,A=1.000000))
   bExpandedInDesigner=True
   DisplayLabel="TextBlock_4"
End Object
Begin Object Class=/Script/UMG.HorizontalBox Name="HorizontalBox_3"
   Begin Object Class=/Script/UMG.HorizontalBoxSlot Name="HorizontalBoxSlot_0"
   End Object
   Begin Object Name="HorizontalBoxSlot_0"
      Size=(SizeRule=Fill)
      Parent=HorizontalBox'"HorizontalBox_3"'
      Content=Button'"ButtonKillAll"'
   End Object
   Slots(0)=HorizontalBoxSlot'"HorizontalBoxSlot_0"'
   bExpandedInDesigner=True
End Object
Begin Object Class=/Script/UMG.Button Name="ButtonKillAll"
   Begin Object Class=/Script/UMG.ButtonSlot Name="ButtonSlot_0"
   End Object
   Begin Object Name="ButtonSlot_0"
      Parent=Button'"ButtonKillAll"'
      Content=TextBlock'"TextBlock_2"'
   End Object
   Slots(0)=ButtonSlot'"ButtonSlot_0"'
   bExpandedInDesigner=True
   DisplayLabel="ButtonKillAll"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_2"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "ECA595D10115454DB6B7B43EF4114CE4", "Kill all enemies")
   ColorAndOpacity=(SpecifiedColor=(R=0.000000,G=0.000000,B=0.000000,A=1.000000))
   bExpandedInDesigner=True
   DisplayLabel="TextBlock_2"
End Object
Begin Object Class=/Script/UMG.HorizontalBox Name="HorizontalBox_1"
   Begin Object Class=/Script/UMG.HorizontalBoxSlot Name="HorizontalBoxSlot_0"
   End Object
   Begin Object Class=/Script/UMG.HorizontalBoxSlot Name="HorizontalBoxSlot_1"
   End Object
   Begin Object Name="HorizontalBoxSlot_0"
      Parent=HorizontalBox'"HorizontalBox_1"'
      Content=CheckBox'"CheckBoxInvulnerability"'
   End Object
   Begin Object Name="HorizontalBoxSlot_1"
      Parent=HorizontalBox'"HorizontalBox_1"'
      Content=TextBlock'"TextBlock_77"'
   End Object
   Slots(0)=HorizontalBoxSlot'"HorizontalBoxSlot_0"'
   Slots(1)=HorizontalBoxSlot'"HorizontalBoxSlot_1"'
   bExpandedInDesigner=True
End Object
Begin Object Class=/Script/UMG.CheckBox Name="CheckBoxInvulnerability"
   WidgetStyle=(UncheckedImage=(ImageSize=(X=32.000000,Y=32.000000)),UncheckedHoveredImage=(ImageSize=(X=32.000000,Y=32.000000)),UncheckedPressedImage=(ImageSize=(X=32.000000,Y=32.000000)),CheckedImage=(ImageSize=(X=32.000000,Y=32.000000)),CheckedHoveredImage=(ImageSize=(X=32.000000,Y=32.000000)),CheckedPressedImage=(ImageSize=(X=32.000000,Y=32.000000)),UndeterminedImage=(ImageSize=(X=32.000000,Y=32.000000)),UndeterminedHoveredImage=(ImageSize=(X=32.000000,Y=32.000000)),UndeterminedPressedImage=(ImageSize=(X=32.000000,Y=32.000000)))
   DisplayLabel="CheckBoxInvulnerability"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_77"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "2309ACDBA92F4CB7BF89D8C77777C3FB", "Invulnerability")
End Object
Begin Object Class=/Script/UMG.HorizontalBox Name="HorizontalBox_4"
   Begin Object Class=/Script/UMG.HorizontalBoxSlot Name="HorizontalBoxSlot_0"
   End Object
   Begin Object Class=/Script/UMG.HorizontalBoxSlot Name="HorizontalBoxSlot_1"
   End Object
   Begin Object Name="HorizontalBoxSlot_0"
      Parent=HorizontalBox'"HorizontalBox_4"'
      Content=CheckBox'"CheckBoxFullBright"'
   End Object
   Begin Object Name="HorizontalBoxSlot_1"
      Parent=HorizontalBox'"HorizontalBox_4"'
      Content=TextBlock'"TextBlock_3"'
   End Object
   Slots(0)=HorizontalBoxSlot'"HorizontalBoxSlot_0"'
   Slots(1)=HorizontalBoxSlot'"HorizontalBoxSlot_1"'
   bExpandedInDesigner=True
End Object
Begin Object Class=/Script/UMG.CheckBox Name="CheckBoxFullBright"
   WidgetStyle=(UncheckedImage=(ImageSize=(X=32.000000,Y=32.000000)),UncheckedHoveredImage=(ImageSize=(X=32.000000,Y=32.000000)),UncheckedPressedImage=(ImageSize=(X=32.000000,Y=32.000000)),CheckedImage=(ImageSize=(X=32.000000,Y=32.000000)),CheckedHoveredImage=(ImageSize=(X=32.000000,Y=32.000000)),CheckedPressedImage=(ImageSize=(X=32.000000,Y=32.000000)),UndeterminedImage=(ImageSize=(X=32.000000,Y=32.000000)),UndeterminedHoveredImage=(ImageSize=(X=32.000000,Y=32.000000)),UndeterminedPressedImage=(ImageSize=(X=32.000000,Y=32.000000)))
   DisplayLabel="CheckBoxFullBright"
End Object
Begin Object Class=/Script/UMG.TextBlock Name="TextBlock_3"
   Text=NSLOCTEXT("[19E044D9EFAC4C838F84B9E215B59561]", "C3D372162F0D4652803AC592D1A2E71D", "Full Bright")
End Object
Begin Object Class=/Script/UMG.NamedSlot Name="NamedSlotSpawnEnemies"
   DisplayLabel="NamedSlotSpawnEnemies"
End Object
