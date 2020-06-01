//
// Buffer.h
//


#pragma once


class CBuffer
{
// Construction
public:
	CBuffer(DWORD* pLimit = NULL);
	virtual ~CBuffer();

// Attributes
public:
	CBuffer*		m_pNext;
	BYTE*			m_pBuffer;
	DWORD			m_nLength;
	DWORD			m_nBuffer;
	
// Operations
public:
	//往buffer  增加数据
	void	Add(const void* pData, DWORD nLength);
	void	Insert(DWORD nOffset, const void* pData, DWORD nLength);

	void	Remove(DWORD nLength);
	void	Clear();
	
	void	Print(LPCSTR pszText);
	void	Print(LPCWSTR pszText, UINT nCodePage = CP_ACP);
	
	DWORD	AddBuffer(CBuffer* pBuffer, DWORD nLength = 0xFFFFFFFF);
	void	AddReversed(const void* pData, DWORD nLength);
	void	Prefix(LPCSTR pszText);
	void	EnsureBuffer(DWORD nLength);
	
public:
	CString	ReadString(DWORD nBytes, UINT nCodePage = CP_ACP);
	BOOL	ReadLine(CString& strLine, BOOL bPeek = FALSE, UINT nCodePage = CP_ACP);
	BOOL	StartsWith(LPCSTR pszString, BOOL bRemove = FALSE);
	DWORD	Receive(SOCKET hSocket);
	DWORD	Send(SOCKET hSocket);
	void	WriteDIME(DWORD nFlags, LPCSTR pszID, LPCSTR pszType, LPCVOID pBody, DWORD nBody);
	BOOL	ReadDIME(DWORD* pnFlags, CString* psID, CString* psType, DWORD* pnBody);

public:
	static void ReverseBuffer(const void* pInput, void* pOutput, DWORD nLength);

};

