-- | SID sound chip registers ($D400-$D41C).
module Target.C64.Mem.SID where

import Data.Word (Word16)

-- Voice 1
sidV1FreqLo, sidV1FreqHi :: Word16
sidV1FreqLo = 0xD400
sidV1FreqHi = 0xD401

sidV1PulseWidthLo, sidV1PulseWidthHi :: Word16
sidV1PulseWidthLo = 0xD402
sidV1PulseWidthHi = 0xD403

sidV1Control :: Word16
sidV1Control = 0xD404

sidV1AttackDecay :: Word16
sidV1AttackDecay = 0xD405

sidV1SustainRelease :: Word16
sidV1SustainRelease = 0xD406

-- Voice 2
sidV2FreqLo, sidV2FreqHi :: Word16
sidV2FreqLo = 0xD407
sidV2FreqHi = 0xD408

sidV2PulseWidthLo, sidV2PulseWidthHi :: Word16
sidV2PulseWidthLo = 0xD409
sidV2PulseWidthHi = 0xD40A

sidV2Control :: Word16
sidV2Control = 0xD40B

sidV2AttackDecay :: Word16
sidV2AttackDecay = 0xD40C

sidV2SustainRelease :: Word16
sidV2SustainRelease = 0xD40D

-- Voice 3
sidV3FreqLo, sidV3FreqHi :: Word16
sidV3FreqLo = 0xD40E
sidV3FreqHi = 0xD40F

sidV3PulseWidthLo, sidV3PulseWidthHi :: Word16
sidV3PulseWidthLo = 0xD410
sidV3PulseWidthHi = 0xD411

sidV3Control :: Word16
sidV3Control = 0xD412

sidV3AttackDecay :: Word16
sidV3AttackDecay = 0xD413

sidV3SustainRelease :: Word16
sidV3SustainRelease = 0xD414

-- Filter
sidFilterCutoffLo :: Word16
sidFilterCutoffLo = 0xD415

sidFilterCutoffHi :: Word16
sidFilterCutoffHi = 0xD416

sidFilterResonanceRouting :: Word16
sidFilterResonanceRouting = 0xD417

-- Volume and filter mode
sidVolumeFilterMode :: Word16
sidVolumeFilterMode = 0xD418

-- Misc (read-only)
sidPaddleX :: Word16
sidPaddleX = 0xD419

sidPaddleY :: Word16
sidPaddleY = 0xD41A

sidOsc3Random :: Word16
sidOsc3Random = 0xD41B

sidEnv3Output :: Word16
sidEnv3Output = 0xD41C
