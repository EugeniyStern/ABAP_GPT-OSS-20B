sap.ui.define([
    "sap/ui/model/json/JSONModel"
], function(JSONModel) {
    "use strict";

    return {
        createMessageModel: function() {
            var oModel = new JSONModel({
                messages: [],
                sessionId: null,
                sapLogon: null,
                nickname: null,
                sessions: []
            });
            
            // Устанавливаем endpoint для работы с SAP backend
            oModel.setDefaultBindingMode(sap.ui.model.BindingMode.TwoWay);
            
            return oModel;
        },
        
        /**
         * Загружает историю чата из SAP для указанной сессии
         * Использует упрощенный подход - отправляет пустое сообщение с session_id
         * для получения истории (если backend поддерживает)
         * @param {string} sSessionId - ID сессии
         * @param {string} sSapLogon - SAP логин пользователя
         * @param {string} sNickname - Никнейм пользователя
         * @returns {Promise} Promise с данными истории
         */
        loadChatHistory: function(sSessionId, sSapLogon, sNickname) {
            return new Promise(function(resolve, reject) {
                if (!sSessionId) {
                    resolve([]);
                    return;
                }
                
                var sUrl = "/sap/yllm/session";
                var oPayload = {
                    sap_logon: sSapLogon || "",
                    nickname: sNickname || "",
                    session_id: sSessionId
                };
                
                jQuery.ajax({
                    url: sUrl,
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                        "action": "history"
                    },
                    data: JSON.stringify(oPayload),
                    success: function(oResponse) {
                        // oResponse должен быть массивом сообщений
                        if (Array.isArray(oResponse)) {
                            resolve(oResponse);
                        } else {
                            resolve([]);
                        }
                    },
                    error: function(oError) {
                        console.error("Ошибка загрузки истории:", oError);
                        reject(oError);
                    }
                });
            });
        },
        
        /**
         * Отправляет сообщение в SAP через endpoint /sap/yllm
         * @param {string} sMessage - Текст сообщения
         * @param {string} sSessionId - ID сессии (опционально)
         * @param {string} sSapLogon - SAP логин пользователя
         * @param {string} sNickname - Никнейм пользователя
         * @returns {Promise} Promise с ответом от сервера
         */
        sendMessageToSAP: function(sMessage, sSessionId, sSapLogon, sNickname) {
            return new Promise(function(resolve, reject) {
                // Используем path /sap/yllm/chat/message для определения команды
                // или можно использовать /sap/yllm с header/query параметром command
                var sUrl = "/sap/yllm/chat/message";
                var oPayload = {
                    sap_logon: sSapLogon || "",
                    nickname: sNickname || "",
                    message: sMessage
                };
                
                if (sSessionId) {
                    oPayload.session_id = sSessionId;
                }
                
                jQuery.ajax({
                    url: sUrl,
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json"
                    },
                    data: JSON.stringify(oPayload),
                    success: function(oResponse) {
                        resolve(oResponse);
                    },
                    error: function(oError) {
                        reject(oError);
                    }
                });
            });
        },
        
        /**
         * Создает новую сессию в SAP
         * @param {string} sSapLogon - SAP логин пользователя
         * @param {string} sNickname - Никнейм пользователя
         * @param {string} sSessionTitle - Заголовок сессии (опционально)
         * @returns {Promise} Promise с ответом от сервера (session_id)
         */
        createSession: function(sSapLogon, sNickname, sSessionTitle) {
            return new Promise(function(resolve, reject) {
                var sUrl = "/sap/yllm/session";
                var oPayload = {
                    sap_logon: sSapLogon || "",
                    nickname: sNickname || ""
                };
                
                if (sSessionTitle) {
                    oPayload.session_title = sSessionTitle;
                }
                
                jQuery.ajax({
                    url: sUrl,
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                        "action": "create"
                    },
                    data: JSON.stringify(oPayload),
                    success: function(oResponse) {
                        resolve(oResponse);
                    },
                    error: function(oError) {
                        reject(oError);
                    }
                });
            });
        },
        
        /**
         * Получает список сессий пользователя из SAP
         * @param {string} sSapLogon - SAP логин пользователя
         * @param {string} sNickname - Никнейм пользователя
         * @returns {Promise} Promise с массивом сессий
         */
        getUserSessions: function(sSapLogon, sNickname) {
            return new Promise(function(resolve, reject) {
                var sUrl = "/sap/yllm/session";
                var oPayload = {
                    sap_logon: sSapLogon || "",
                    nickname: sNickname || ""
                };
                
                jQuery.ajax({
                    url: sUrl,
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                        "action": "list"
                    },
                    data: JSON.stringify(oPayload),
                    success: function(oResponse) {
                        resolve(oResponse);
                    },
                    error: function(oError) {
                        reject(oError);
                    }
                });
            });
        }
    };
});

