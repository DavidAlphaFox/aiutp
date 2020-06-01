//
// Buffer.cpp
//

#include "StdAfx.h"
#include "SourceAPP.h"
#include "Settings.h"
#include "Buffer.h"
#include "Packet.h"
#include "Statistics.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

#define TEMP_BUFFER			4096
#define BLOCK_SIZE			1024
#define BLOCK_MASK			0xFFFFFC00


///////////////////////////////////////////////////////////////////////////////
// CBuffer construction

CBuffer::CBuffer(DWORD* pLimit)
{
	m_pNext		= NULL;

	m_pBuffer	= NULL;
	m_nBuffer	= 0;
	m_nLength	= 0;
}

CBuffer::~CBuffer()
{
	if ( m_pBuffer ) free( m_pBuffer );
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer add

void CBuffer::Add(const void * pData, DWORD nLength)
{
	if ( m_nLength + nLength > m_nBuffer )
	{
		m_nBuffer = m_nLength + nLength;
		m_nBuffer = ( m_nBuffer + BLOCK_SIZE - 1 ) & BLOCK_MASK;
		m_pBuffer = (BYTE*)realloc( m_pBuffer, m_nBuffer );
	}
	else if ( m_nBuffer > 0x80000 && m_nLength + nLength < 0x40000 )
	{
		m_nBuffer = 0x40000;
		m_pBuffer = (BYTE*)realloc( m_pBuffer, m_nBuffer );
	}

	CopyMemory( m_pBuffer + m_nLength, pData, nLength );
	m_nLength += nLength;
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer insert

void CBuffer::Insert(DWORD nOffset, const void * pData, DWORD nLength)
{
	if ( m_nLength + nLength > m_nBuffer )
	{
		m_nBuffer = m_nLength + nLength;
		m_nBuffer = ( m_nBuffer + BLOCK_SIZE - 1 ) & BLOCK_MASK;
		m_pBuffer = (BYTE*)realloc( m_pBuffer, m_nBuffer );
	}
	else if ( m_nBuffer > 0x80000 && m_nLength + nLength < 0x40000 )
	{
		m_nBuffer = 0x40000;
		m_pBuffer = (BYTE*)realloc( m_pBuffer, m_nBuffer );
	}
	
	MoveMemory( m_pBuffer + nOffset + nLength, m_pBuffer + nOffset, m_nLength - nOffset );
	CopyMemory( m_pBuffer + nOffset, pData, nLength );
	m_nLength += nLength;
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer remove

void CBuffer::Remove(DWORD nLength)
{
	if ( nLength > m_nLength || nLength == 0 ) return;
	m_nLength -= nLength;
	MoveMemory( m_pBuffer, m_pBuffer + nLength, m_nLength );
}

void CBuffer::Clear()
{
	m_nLength = 0;
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer add utilities

void CBuffer::Print(LPCSTR pszText)
{
	if ( pszText == NULL ) return;
	Add( (void*)pszText, strlen( pszText ) );
}

void CBuffer::Print(LPCWSTR pszText, UINT nCodePage)
{
	if ( pszText == NULL ) return;
	int nLength = wcslen(pszText);
	int nBytes = WideCharToMultiByte( nCodePage, 0, pszText, nLength, NULL, 0, NULL, NULL );
	EnsureBuffer( (DWORD)nBytes );
	WideCharToMultiByte( nCodePage, 0, pszText, nLength, (LPSTR)( m_pBuffer + m_nLength ), nBytes, NULL, NULL );
	m_nLength += nBytes;
}

DWORD CBuffer::AddBuffer(CBuffer* pBuffer, DWORD nLength)
{
	nLength = nLength < 0xFFFFFFFF ? ( min( pBuffer->m_nLength, nLength ) ) : pBuffer->m_nLength;
	Add( pBuffer->m_pBuffer, nLength );
	pBuffer->Remove( nLength );
	return nLength;
}

void CBuffer::AddReversed(const void * pData, DWORD nLength)
{
	EnsureBuffer( nLength );
	ReverseBuffer( pData, m_pBuffer + m_nLength, nLength );
	m_nLength += nLength;
}

void CBuffer::Prefix(LPCSTR pszText)
{
	if ( NULL == pszText ) return;
	Insert( 0, (void*)pszText, strlen( pszText ) );
}

void CBuffer::EnsureBuffer(DWORD nLength)
{
	if ( m_nBuffer - m_nLength >= nLength ) return;
	
	m_nBuffer = m_nLength + nLength;
	m_nBuffer = ( m_nBuffer + BLOCK_SIZE - 1 ) & BLOCK_MASK;
	m_pBuffer = (BYTE*)realloc( m_pBuffer, m_nBuffer );
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer read string helper

CString CBuffer::ReadString(DWORD nBytes, UINT nCodePage)
{
	CString str;
	
	int nSource = (int)min( nBytes, m_nLength );
	int nLength = MultiByteToWideChar( nCodePage, 0, (LPCSTR)m_pBuffer, nSource, NULL, 0 );
#ifdef _UNICODE
	MultiByteToWideChar( nCodePage, 0, (LPCSTR)m_pBuffer, nSource, str.GetBuffer( nLength ), nLength );
	str.ReleaseBuffer( nLength );
#else
	LPWSTR pszWide = new WCHAR[ nLength + 1 ];
	MultiByteToWideChar( nCodePage, 0, (LPCSTR)m_pBuffer, nSource, pszWide, nLength );
	pszWide[ nLength ] = 0;
	str = pszWide;
	delete [] pszWide;
#endif
	
	return str;
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer read line helper

BOOL CBuffer::ReadLine(CString& strLine, BOOL bPeek, UINT nCodePage)
{
	strLine.Empty();
	if ( ! m_nLength ) return FALSE;
	
	for ( DWORD nLength = 0 ; nLength < m_nLength ; nLength++ )
	{
		if ( m_pBuffer[ nLength ] == '\n' ) break;
	}
	
	if ( nLength >= m_nLength ) return FALSE;
	
#ifdef _UNICODE
	int nWide = MultiByteToWideChar( nCodePage, 0, (LPCSTR)m_pBuffer, nLength, NULL, 0 );
    MultiByteToWideChar( nCodePage, 0, (LPCSTR)m_pBuffer, nLength, strLine.GetBuffer( nWide ), nWide );
	strLine.ReleaseBuffer( nWide );
	int nCR = strLine.ReverseFind( '\r' );
	if ( nCR >= 0 ) strLine.Truncate( nCR );
#else
	CopyMemory( strLine.GetBuffer( nLength ), m_pBuffer, nLength );
	strLine.ReleaseBuffer( ( nLength > 0 && m_pBuffer[ nLength - 1 ] == '\r' ) ? nLength - 1 : nLength );
#endif
	
	if ( ! bPeek ) Remove( nLength + 1 );
	
	return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer starts with helper

BOOL CBuffer::StartsWith(LPCSTR pszString, BOOL bRemove)
{
	if ( m_nLength < (int)strlen( pszString ) ) return FALSE;
	
	if ( strncmp( (LPCSTR)m_pBuffer, (LPCSTR)pszString, strlen( pszString ) ) )
		return FALSE;
	
	if ( bRemove ) Remove( strlen( pszString ) );
	
	return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer socket receive

DWORD CBuffer::Receive(SOCKET hSocket)
{
	BYTE pData[TEMP_BUFFER];
	DWORD nTotal = 0;
	
	while ( TRUE )
	{
		int nLength = recv( hSocket, (char*)pData, TEMP_BUFFER, 0 );
		if ( nLength <= 0 ) break;
		
		Add( pData, nLength );
		
		nTotal += nLength;
	}
	
	Statistics.Current.Bandwidth.Incoming += nTotal;
	
	return nTotal;
}

///////////////////////////////////////////////////////////////////////////////
// CBuffer socket send

DWORD CBuffer::Send(SOCKET hSocket)
{
	DWORD nTotal = 0;
	
	while ( m_nLength )
	{
		int nLength = send( hSocket, (char*)m_pBuffer, m_nLength, 0 );
		if ( nLength <= 0 ) break;
		
		Remove( nLength );
		
		nTotal += nLength;
	}
	
	Statistics.Current.Bandwidth.Outgoing += nTotal;
	
	return nTotal;
}


//////////////////////////////////////////////////////////////////////
// CBuffer reverse buffer

void CBuffer::ReverseBuffer(const void* pInput, void* pOutput, DWORD nLength)
{
	const DWORD* pInputWords	= (const DWORD*)( (const BYTE*)pInput + nLength );
	DWORD* pOutputWords			= (DWORD*)( pOutput );
	register DWORD nTemp;
	
	while ( nLength > 4 )
	{
		nTemp = *--pInputWords;
		*pOutputWords++ = SWAP_LONG( nTemp );
		nLength -= 4;
	}
	
	if ( nLength )
	{
		const BYTE* pInputBytes	= (const BYTE*)pInputWords;
		BYTE* pOutputBytes		= (BYTE*)pOutputWords;
		
		while ( nLength-- )
		{
			*pOutputBytes++ = *--pInputBytes;
		}
	}
}

//////////////////////////////////////////////////////////////////////
// CBuffer DIME handling

void CBuffer::WriteDIME(DWORD nFlags, LPCSTR pszID, LPCSTR pszType, LPCVOID pBody, DWORD nBody)
{
	EnsureBuffer( 12 );
	BYTE* pOut = m_pBuffer + m_nLength;
	DWORD nPad;
	
	*pOut++ = 0x08 | ( nFlags & 1 ? 4 : 0 ) | ( nFlags & 2 ? 2 : 0 );
	*pOut++ = strchr( pszType, ':' ) ? 0x20 : 0x10;
	*pOut++ = 0x00; *pOut++ = 0x00;
	*pOut++ = ( ( strlen( pszID ) & 0xFF00 ) >> 8 );
	*pOut++ = ( strlen( pszID ) & 0xFF );
	*pOut++ = ( ( strlen( pszType ) & 0xFF00 ) >> 8 );
	*pOut++ = ( strlen( pszType ) & 0xFF );
	*pOut++ = (BYTE)( ( nBody & 0xFF000000 ) >> 24 );
	*pOut++ = (BYTE)( ( nBody & 0x00FF0000 ) >> 16 );
	*pOut++ = (BYTE)( ( nBody & 0x0000FF00 ) >> 8 );
	*pOut++ = (BYTE)( nBody & 0x000000FF );
	m_nLength += 12;
	
	Print( pszID );
	for ( nPad = strlen( pszID ) ; nPad & 3 ; nPad++ ) Add( "", 1 );
	Print( pszType );
	for ( nPad = strlen( pszType ) ; nPad & 3 ; nPad++ ) Add( "", 1 );
	
	if ( pBody != NULL )
	{
		Add( pBody, nBody );
		for ( nPad = nBody ; nPad & 3 ; nPad++ ) Add( "", 1 );
	}
}

BOOL CBuffer::ReadDIME(DWORD* pnFlags, CString* psID, CString* psType, DWORD* pnBody)
{
	if ( m_nLength < 12 ) return FALSE;
	BYTE* pIn = m_pBuffer;
	
	if ( ( *pIn & 0xF8 ) != 0x08 ) return FALSE;
	
	if ( pnFlags != NULL )
	{
		*pnFlags = 0;
		if ( *pIn & 4 ) *pnFlags |= 1;
		if ( *pIn & 2 ) *pnFlags |= 2;
	}
	
	pIn++;

	if ( *pIn != 0x10 && *pIn != 0x20 ) return FALSE;
	pIn++;
	
	if ( *pIn++ != 0x00 ) return FALSE;
	if ( *pIn++ != 0x00 ) return FALSE;
	
	ASSERT( pnBody != NULL );
	WORD nID	= ( pIn[0] << 8 ) + pIn[1]; pIn += 2;
	WORD nType	= ( pIn[0] << 8 ) + pIn[1]; pIn += 2;
	*pnBody		= ( pIn[0] << 24 ) + ( pIn[1] << 16 ) + ( pIn[2] << 8 ) + pIn[3];
	pIn += 4;
	
	DWORD nSkip = 12 + ( ( nID + 3 ) & ~3 ) + ( ( nType + 3 ) & ~3 );
	if ( m_nLength < nSkip + ( ( *pnBody + 3 ) & ~3 ) ) return FALSE;
	
	ASSERT( psID != NULL );
	LPSTR pszID = new CHAR[ nID + 1 ];
	CopyMemory( pszID, pIn, nID );
	pszID[ nID ] = 0;
	*psID = pszID;
	delete [] pszID;
	pIn += nID;
	while ( nID++ & 3 ) pIn++;
	
	ASSERT( psType != NULL );
	LPSTR pszType = new CHAR[ nType + 1 ];
	CopyMemory( pszType, pIn, nType );
	pszType[ nType ] = 0;
	*psType = pszType;
	delete [] pszType;
	pIn += nType;
	while ( nType++ & 3 ) pIn++;
	
	Remove( nSkip );
	
	return TRUE;
}
