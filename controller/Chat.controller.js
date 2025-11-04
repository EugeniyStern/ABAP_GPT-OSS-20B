sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel"
], function(Controller, JSONModel) {
    "use strict";

    return Controller.extend("com.example.chat.controller.Chat", {
        onInit: function() {
            var oView = this.getView();
            var oComponent = this.getOwnerComponent();
            
            // Получаем модель из компонента или создаем новую
            var oModel = oComponent.getModel("messages");
            
            if (!oModel) {
                // Если модель не найдена, создаем её
                sap.ui.require(["com/example/chat/model/models"], function(models) {
                    var oNewModel = models.createMessageModel();
                    oComponent.setModel(oNewModel, "messages");
                    oView.setModel(oNewModel, "messages");
                });
            } else {
                // Устанавливаем модель в view для обеспечения привязки
                oView.setModel(oModel, "messages");
                
                // Проверяем данные
                var oData = oModel.getData();
                if (!oData || !oData.messages) {
                    // Модель не содержит массива messages
                }
            }
            
        },

        onAfterRendering: function() {
            // Добавляем обработчик для отправки по Ctrl+Enter после рендеринга
            var oTextArea = this.byId("messageInput");
            if (oTextArea) {
                oTextArea.attachBrowserEvent("keydown", function(oEvent) {
                    if (oEvent.ctrlKey && oEvent.keyCode === 13) { // Ctrl+Enter
                        oEvent.preventDefault();
                        this.onSendMessage();
                    }
                }.bind(this));
            }
        },

        onActionPressed: function(oEvent) {
            var oAction = oEvent.getSource();
            var sKey = oAction.getKey();
            var sText = oAction.getText();
            sap.m.MessageToast.show("Нажата кнопка: " + sText + " (Key: " + sKey + ")");
        },

        onSendMessage: function() {
            var oInput = this.byId("messageInput");
            var sMessageText = oInput.getValue();
            
            if (sMessageText.trim() === "") {
                sap.m.MessageToast.show("Пожалуйста, введите сообщение");
                return;
            }

            var oModel = this.getView().getModel("messages");
            var aMessages = oModel.getData().messages || [];
            
            var oNewMessage = {
                sender: "Вы",
                text: sMessageText,
                timestamp: new Date().toLocaleString("ru-RU"),
                icon: "sap-icon://user",
                Actions: [
                    { Text: "Action 1", Key: "action1" },
                    { Text: "Action 2", Key: "action2" }
                ]
            };
            
            aMessages.push(oNewMessage);
            oModel.setData({ messages: aMessages });
            oModel.refresh();
            
            oInput.setValue("");
            sap.m.MessageToast.show("Сообщение отправлено");
        }
    });
});

