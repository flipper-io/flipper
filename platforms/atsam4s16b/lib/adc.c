/* ----------------------------------------------------------------------------
 *         ATMEL Microcontroller Software Support
 * ----------------------------------------------------------------------------
 * Copyright (c) 2010, Atmel Corporation
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the disclaimer below.
 *
 * Atmel's name may not be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * DISCLAIMER: THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE
 * DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ----------------------------------------------------------------------------
 */

/** \addtogroup adc_module Working with ADC
 * \ingroup peripherals_module
 * The ADC driver provides the interface to configure and use the ADC peripheral.
 * \n
 *
 * It converts the analog input to digital format. The converted result could be
 * 12bit or 10bit. The ADC supports up to 16 analog lines.
 *
 * To Enable a ADC conversion,the user has to follow these few steps:
 * <ul>
 * <li> Select an appropriate reference voltage on ADVREF   </li>
 * <li> Configure the ADC according to its requirements and special needs,which
 * could be  broken down into several parts:
 * -#   Select the resolution by setting or clearing ADC_MR_LOWRES bit in
 *      ADC_MR (Mode Register)
 * -#   Set ADC clock by setting ADC_MR_PRESCAL bits in ADC_MR, the clock is
 *      calculated with ADCClock = MCK / ( (PRESCAL+1) * 2 )
 * -#   Set Startup Time,Tracking Clock cycles and Transfer Clock respectively
 *      in ADC_MR.
 </li>
 * <li> Start conversion by setting ADC_CR_START in ADC_CR. </li>
 * </ul>
 *
 * For more accurate information, please look at the ADC section of the
 * Datasheet.
 *
 * Related files :\n
 * \ref adc.c\n
 * \ref adc.h\n
 */
/*@{*/
/*@}*/
/**
 * \file
 *
 * Implementation of Analog-to-Digital Converter (ADC).
 *
 */
/*----------------------------------------------------------------------------
 *        Headers
 *----------------------------------------------------------------------------*/

#include "platform/chip.h"

/*----------------------------------------------------------------------------
 *        Exported functions
 *----------------------------------------------------------------------------*/

/**
 * \brief Initialize the ADC controller
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwID ADC Index
 */
extern void ADC_Initialize( Adc* pAdc, uint32_t dwID )
{
    /* Enable peripheral clock*/
    PMC_EnablePeripheral(dwID);

    /*  Reset the controller */
    pAdc->ADC_CR = ADC_CR_SWRST;

    /* Reset Mode Register */
    pAdc->ADC_MR = 0;

    /* Reset PDC transfer */
    pAdc->ADC_PTCR = (ADC_PTCR_RXTDIS | ADC_PTCR_TXTDIS);
    pAdc->ADC_RCR = 0;
    pAdc->ADC_RNCR = 0;
    pAdc->ADC_TCR = 0;
    pAdc->ADC_TNCR = 0;
}

/**
 * \brief Set ADC clock.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwPres prescal value
 * \param dwMck Board MCK (Hz)
 *
 * \return ADC clock
 */

extern uint32_t ADC_SetClock( Adc* pAdc, uint32_t dwPres, uint32_t dwMck )
{
    uint32_t dwMr;
    uint32_t dwAdcClock;

    dwMr = pAdc->ADC_MR;
    dwMr = (dwMr & ~ADC_MR_PRESCAL_Msk) | ADC_MR_PRESCAL( dwPres );
    pAdc->ADC_MR |= dwMr;

    /* Formula: ADCClock = MCK / ( (PRESCAL+1) * 2 ) */
    dwAdcClock = dwMck / ( (dwPres + 1) * 2 );

    return dwAdcClock;
}

/**
 * \brief Set ADC timing.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwStartup startup value
 * \param dwTransfer transfer value
 * \param dwTracking tracking value
 * \param dwSettling settling value
 */
extern void ADC_SetTiming( Adc* pAdc, uint32_t dwStartup, uint32_t dwTransfer,
                                      uint32_t dwTracking, uint32_t dwSettling )
{
    uint32_t dwMr;

    dwMr = pAdc->ADC_MR;
    dwMr &= (~ADC_MR_STARTUP_Msk) & (~ADC_MR_TRACKTIM_Msk) &
            (~ADC_MR_TRANSFER_Msk) & (~ADC_MR_SETTLING_Msk);

    /* Formula:
     *     Startup  Time = startup value / ADCClock
     *     Transfer Time = (TRANSFER * 2 + 3) / ADCClock
     *     Tracking Time = (TRACKTIM + 1) / ADCClock
     *     Settling Time = settling value / ADCClock
     */
    dwMr |= dwStartup | dwTransfer | dwTracking | dwSettling;
    pAdc->ADC_MR |= dwMr;
}

/**
 * \brief Set ADC trigger.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param bEnDis Enable/Disable hardware trigger
 * \param dwTrgSel Trigger selection
 */
extern void ADC_SetTrigger( Adc* pAdc, uint32_t bEnDis, uint32_t dwTrgSel )
{
    uint32_t dwMr;

    dwMr = pAdc->ADC_MR;

    if ( bEnDis )
    {
        dwMr |= ADC_MR_TRGEN;
    }
    else
    {
        dwMr &= ~ADC_MR_TRGEN;
    }
    dwMr &= ~ADC_MR_TRGSEL_Msk;
    dwMr |= dwTrgSel;
    pAdc->ADC_MR |= dwMr;
}

/**
 * \brief Set free run mode.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param bEnDis Enable/Disable free run mode.
 */
extern void ADC_SetFreeRunMode( Adc* pAdc, uint32_t bEnDis )
{
    if ( bEnDis )
    {
        pAdc->ADC_MR |= ADC_MR_FREERUN;
    }
    else
    {
        pAdc->ADC_MR &= ~ADC_MR_FREERUN;
    }
}


/**
 * \brief Enable/Disable low resolution.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param bEnDis Enable/Disable low resolution.
 */
extern void ADC_SetLowResolution( Adc* pAdc, uint32_t bEnDis )
{
    if ( bEnDis )
    {
        pAdc->ADC_MR |= ADC_MR_LOWRES;
    }
    else
    {
        pAdc->ADC_MR &= ~ADC_MR_LOWRES;
    }
}

/**
 * \brief Enable/Disable sleep mode.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param bEnDis Enable/Disable sleep mode.
 */
extern void ADC_SetSleepMode( Adc *pAdc, uint8_t bEnDis )
{
    if ( bEnDis )
    {
        pAdc->ADC_MR |=  ADC_MR_SLEEP;
    }
    else
    {
        pAdc->ADC_MR &= ~ADC_MR_SLEEP;
    }
}

/**
 * \brief Enable/Disable fast wake up.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param bEnDis Enable/Disable fast wake up in sleep mode.
 */
extern void ADC_SetFastWakeup( Adc *pAdc, uint8_t bEnDis )
{
    if ( bEnDis )
    {
        pAdc->ADC_MR |=  ADC_MR_FWUP;
    }
    else
    {
        pAdc->ADC_MR &= ~ADC_MR_FWUP;
    }
}

/**
 * \brief Enable/Disable seqnence mode.
 *
 * \param pAdc  Pointer to an Adc instance.
 * \param bEnDis Enable/Disable seqnence mode.
 */
extern void ADC_SetSequenceMode( Adc *pAdc, uint8_t bEnDis )
{
    if ( bEnDis )
    {
        /* User Sequence Mode: The sequence respects what is defined in
        ADC_SEQR1 and ADC_SEQR2 */
        pAdc->ADC_MR |=  ADC_MR_USEQ;
    }
    else
    {
        /* Normal Mode: The controller converts channels in a simple numeric order. */
        pAdc->ADC_MR &= ~ADC_MR_USEQ;
    }
}

/**
 * \brief Set channel sequence.
 *
 * \param pAdc   Pointer to an Adc instance.
 * \param dwSEQ1 Sequence 1 ~ 8  channel number.
 * \param dwSEQ2 Sequence 9 ~ 16 channel number.
 */
extern void ADC_SetSequence( Adc *pAdc, uint32_t dwSEQ1, uint32_t dwSEQ2 )
{
    pAdc->ADC_SEQR1 = dwSEQ1;
    pAdc->ADC_SEQR2 = dwSEQ2;
}

/**
 * \brief Set channel sequence by given channel list.
 *
 * \param pAdc    Pointer to an Adc instance.
 * \param ucChList Channel list.
 * \param ucNumCh  Number of channels in list.
 */
extern void ADC_SetSequenceByList( Adc *pAdc, uint8_t ucChList[], uint8_t ucNumCh )
{
    uint8_t i;
    uint8_t ucShift;

    pAdc->ADC_SEQR1 = 0;
    for (i = 0, ucShift = 0; i < 8; i ++, ucShift += 4)
    {
        if (i >= ucNumCh) return;
        pAdc->ADC_SEQR1 |= ucChList[i] << ucShift;

    }
    pAdc->ADC_SEQR2 = 0;
    for (ucShift = 0; i < 16; i ++, ucShift += 4)
    {
        if (i >= ucNumCh) return;
        pAdc->ADC_SEQR2 |= ucChList[i] << ucShift;
    }
}

/**
 * \brief Set analog change.
 * IF enabled, it allows different analog settings for each channel,
 * otherwise, DIFF0, GAIN0 and OFF0 are used for all channels.
 *
 * \param pAdc   Pointer to an Adc instance.
 * \param bEnDis Enable/Disable.
 */
extern void ADC_SetAnalogChange( Adc *pAdc, uint8_t bEnDis )
{
    if ( bEnDis )
    {
        pAdc->ADC_MR |=  ADC_MR_ANACH;
    }
    else
    {
        pAdc->ADC_MR &= ~ADC_MR_ANACH;
    }
}

/**
 * \brief Set "TAG" mode, show channel number in last data or not.
 *
 * \param pAdc   Pointer to an Adc instance.
 * \param bEnDis Enable/Disable TAG value.
 */
extern void ADC_SetTagEnable( Adc *pAdc, uint8_t bEnDis )
{
    if ( bEnDis )
    {
        pAdc->ADC_EMR |=  ADC_EMR_TAG;
    }
    else
    {
        pAdc->ADC_EMR &= ~ADC_EMR_TAG;
    }
}

/**
 * \brief Set compare channel.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwChannel channel number to be set,16 for all channels
 */
extern void ADC_SetCompareChannel( Adc* pAdc, uint32_t dwChannel )
{
    assert( dwChannel <= 16 ) ;

    if ( dwChannel < 16 )
    {
        pAdc->ADC_EMR &= ~(ADC_EMR_CMPALL);
        pAdc->ADC_EMR &= ~(ADC_EMR_CMPSEL_Msk);
        pAdc->ADC_EMR |= (dwChannel << ADC_EMR_CMPSEL_Pos);
    }
    else
    {
        pAdc->ADC_EMR |= ADC_EMR_CMPALL;
    }
}

/**
 * \brief Set compare mode.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwMode compare mode
 */
extern void ADC_SetCompareMode( Adc* pAdc, uint32_t dwMode )
{
    pAdc->ADC_EMR &= ~(ADC_EMR_CMPMODE_Msk);
    pAdc->ADC_EMR |= (dwMode & ADC_EMR_CMPMODE_Msk);
}

/**
 * \brief Set comparsion window.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwHi_Lo Comparison Window
 */
extern void ADC_SetComparisonWindow( Adc* pAdc, uint32_t dwHi_Lo )
{
    pAdc->ADC_CWR = dwHi_Lo ;
}

/**
 * \brief Get startup value.
 */
static uint32_t GetStartupValue( uint32_t dwStartup )
{
    uint32_t dwStartupValue = 0;

    if( dwStartup == 0 )
        dwStartupValue = 0;
    else if( dwStartup == 1 )
        dwStartupValue = 8;
    else if( dwStartup == 2 )
        dwStartupValue = 16;
    else if( dwStartup == 3 )
        dwStartupValue = 24;
    else if( dwStartup == 4 )
        dwStartupValue = 64;
    else if( dwStartup == 5 )
        dwStartupValue = 80;
    else if( dwStartup == 6 )
        dwStartupValue = 96;
    else if( dwStartup == 7 )
        dwStartupValue = 112;
    else if( dwStartup == 8 )
        dwStartupValue = 512;
    else if( dwStartup == 9 )
        dwStartupValue = 576;
    else if( dwStartup == 10 )
        dwStartupValue = 640;
    else if( dwStartup == 11 )
        dwStartupValue = 704;
    else if( dwStartup == 12 )
        dwStartupValue = 768;
    else if( dwStartup == 13 )
        dwStartupValue = 832;
    else if( dwStartup == 14 )
        dwStartupValue = 896;
    else if( dwStartup == 15 )
        dwStartupValue = 960;

    return dwStartupValue;
}

/**
 * \brief Check if ADC configuration is right.
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwMck Board MCK (Hz)
 *
 * \return 0 if check ok, others if not ok.
 */
extern uint8_t ADC_CheckConfiguration( Adc* pAdc, uint32_t dwMck )
{
    uint8_t  bOk = 0;
    uint32_t dwMr;
    uint32_t dwPres;
    uint32_t dwStartup;
    uint32_t dwAdcClock;
    uint32_t dwTemp;

    dwMr = pAdc->ADC_MR;

    dwPres = (dwMr & ADC_MR_PRESCAL_Msk) >> ADC_MR_PRESCAL_Pos;
    /* Formula: ADCClock = MCK / ( (PRESCAL+1) * 2 ) */
    dwAdcClock = dwMck / ( (dwPres + 1) * 2 );
    if (dwAdcClock > ADC_CLOCK_MAX)
    {
        printf("ADC clock is too high (out of specification: %d Hz)\r\n", (int)ADC_CLOCK_MAX);
        bOk = 1;
    }

    dwStartup = (dwMr & ADC_MR_STARTUP_Msk) >> ADC_MR_STARTUP_Pos;
    if (dwMr & ADC_MR_SLEEP_SLEEP)
    {
        if (dwMr & ADC_MR_FREERUN_ON)
        {
            printf("FreeRun mode is forbidden in sleep mode\n\r");
            bOk = 1;
        }
        if( !(dwMr & ADC_MR_FWUP_ON) )
        {
            /* Sleep 40µs */
            dwTemp = ADC_STARTUP_NORMAL_MAX * dwAdcClock / 1000000;
            if( dwTemp > GetStartupValue(dwStartup) )
            {
                printf("Startup time too small: %d, programmed: %d\r\n", (int)dwTemp, (int)(GetStartupValue(dwStartup)));
                bOk = 1;
            }
        }
        else
        {
            if( pAdc->ADC_MR & ADC_MR_FWUP_ON )
            {
                /* Fast Wake Up Sleep Mode: 12µs */
                dwTemp = ADC_STARTUP_FAST_MAX * dwAdcClock / 1000000;
                if( dwTemp > GetStartupValue(dwStartup) )
                {
                    printf("Startup time too small: %d, programmed: %d\r\n", (int)dwTemp, (int)(GetStartupValue(dwStartup)));
                    bOk = 1;
                }
            }
        }
    }

    return bOk;
}

/**
 * \brief Return the Channel Converted Data
 *
 * \param pAdc Pointer to an Adc instance.
 * \param dwChannel channel to get converted value
 */
extern uint32_t ADC_GetConvertedData( Adc* pAdc, uint32_t dwChannel )
{
    uint32_t dwData = 0;

    assert( dwChannel < 16 ) ;

    dwData = pAdc->ADC_CDR[dwChannel];

    return dwData ;
}

/**
 * \brief Read converted data through PDC channel
 *
 * \param pADC the pointer of adc peripheral
 * \param pBuffer the destination buffer
 * \param dwSize the size of the buffer
 */
extern uint32_t ADC_ReadBuffer( Adc* pADC, uint16_t *pwBuffer, uint32_t dwSize )
{
    /* Check if the first PDC bank is free*/
    if ( (pADC->ADC_RCR == 0) && (pADC->ADC_RNCR == 0) )
    {
        pADC->ADC_RPR = (uint32_t)pwBuffer ;
        pADC->ADC_RCR = dwSize ;
        pADC->ADC_PTCR = ADC_PTCR_RXTEN;

        return 1;
    }
    /* Check if the second PDC bank is free*/
    else
    {
        if ( pADC->ADC_RNCR == 0 )
        {
            pADC->ADC_RNPR = (uint32_t)pwBuffer ;
            pADC->ADC_RNCR = dwSize ;

            return 1 ;
        }
        else
        {
            return 0 ;
        }
    }
}

